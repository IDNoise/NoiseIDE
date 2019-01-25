__author__ = 'IDNoise'

import os
import re
import time
import wx
import yaml
from idn_colorschema import ColorSchema
from idn_config import Config
from idn_findreplace import ReplaceInProject, ReplaceInFile
import core
from idn_projectexplorer import ProjectExplorer
from idn_utils import Menu, writeFile, readFile, GetAllFilesInDir, extension
from idn_erlang_utils import IsModule
from idn_erlang_dialogs import ErlangRenameDialog
import codecs


def loadCmds(filePath):
    cmds = []
    if (os.path.exists(filePath)):
        stream = codecs.open(filePath, "r", "utf-8")
        cmds = yaml.load(stream)
    return cmds

class ErlangProjectExplorer(ProjectExplorer):
    def __init__(self, parent, project):
        ProjectExplorer.__init__(self, parent, project)
        self.pathErrors = {}
        self.needHighlight = True

        self.highlightTimer = wx.Timer(self, wx.ID_ANY)
        self.highlightTimer.Start(250)
        self.Bind(wx.EVT_TIMER, self.OnHighlightTimer, self.highlightTimer)

        self.userCmds = []
        for path in [os.path.join(core.MainFrame.cwd, "data", "erlang", "user_cmds.yaml"),
                     os.path.join(core.Project.projectDir, "cmds.yaml")]:
            if os.path.exists(path):
                self.userCmds += loadCmds(path)
        self.ideCmds = loadCmds(os.path.join(core.MainFrame.cwd, "data", "erlang", "ide_cmds.yaml"))

    def CreateMenu(self):
        menu = ProjectExplorer.CreateMenu(self)

        def usertpl(filePath, title):
            return lambda e: self.CreateFromUserTemplate(filePath, title, title + "_default")

        if hasattr(menu, "newMenu"):
            newMenu = menu.newMenu
            newMenu.AppendMenuItem("Module", self, lambda e:
                self.CreateFromTemplate("module.erl", "Module", "module_1"))
            newMenu.AppendMenuItem("Header", self, self.OnMenuNewHeader)
            newMenu.AppendMenuItem("Application", self, self.OnMenuNewApplication)
            tMenu = Menu()
            tMenu.AppendMenuItem("Gen Server", self, lambda e:
                self.CreateFromTemplate("gen_server.erl", "Gen server", "gen_server_1"))
            tMenu.AppendMenuItem("Gen Event", self, lambda e:
                self.CreateFromTemplate("gen_event.erl", "Gen event", "gen_event_1"))
            tMenu.AppendMenuItem("Gen FSM", self, lambda e:
                self.CreateFromTemplate("gen_fsm.erl", "Gen fsm", "gen_fsm_1"))
            tMenu.AppendMenuItem("Supervisor", self, lambda e:
                self.CreateFromTemplate("supervisor.erl", "Supervisor", "supervisor_1"))
            tMenu.AppendMenuItem("Application", self, lambda e:
                self.CreateFromTemplate("application.erl", "Application", "application_1"))
            tMenu.AppendMenuItem("App Src", self, lambda e:
                self.CreateFromTemplate("app.src", "App Src", "application_1", ".app.src", "Enter application name:", "[app]"))

            userTemplateMenu = Menu()

            for path in [os.path.join(core.MainFrame.cwd, "data", "erlang", "templates", "user"),
                         os.path.join(core.Project.projectDir, "templates.yaml")]:
                for templateFile in GetAllFilesInDir(path):
                    filename = os.path.basename(templateFile)
                    title = filename.split(".")[0]
                    userTemplateMenu.AppendMenuItem(title, self, usertpl(templateFile, title))

            tMenu.AppendMenu(wx.ID_ANY, "User", userTemplateMenu)
            newMenu.AppendMenu(wx.ID_ANY, "Template", tMenu)

        dialyzerMenu = Menu()
        dialyzerMenu.AppendMenuItem("Project", self, lambda e: self.project.DialyzeProject())
        if self.eventItem == self.GetRootItem():
            menu.AppendSeparator()
            menu.AppendMenu(wx.ID_ANY, "Dialyzer", dialyzerMenu)
        elif self.GetRootItem() in self.selectedItems:
            pass
        else:
            paths = []
            for item in self.selectedItems:
                paths.append(self.GetPyData(item))

            allFiles = []
            for item in self.selectedItems:
                if self.ItemHasChildren(item):
                    allFiles += self.GetAllItemFiles(item)
                else:
                    allFiles.append(self.GetPyData(item))

            dialyzerMenu.AppendMenuItem("Apps", self, lambda e: self.project.DialyzeApps(paths))
            dialyzerMenu.AppendMenuItem("Modules", self, lambda e: self.project.DialyzeModules(allFiles))
            menu.AppendSeparator()
            menu.AppendMenu(wx.ID_ANY, "Dialyzer", dialyzerMenu)


        self.FillCommandsMenu(menu, "User cmds", self.userCmds)
        self.FillCommandsMenu(menu, "Ide cmds", self.ideCmds)

        return menu

    def FillCommandsMenu(self, menu, title, commands):
        if not commands: return
        def exec_user_cmd(cmd, consoleType):
            return lambda e: self.ExecErlangCmd(cmd, consoleType)
        categories = {}
        userCommandsMenu = Menu()

        for cmd in commands:
            currentMenu = userCommandsMenu
            if 'category' in cmd and cmd['category'] != "":
                cat = cmd['category']
                if not cat in categories:
                    categories[cat] = Menu()
                    userCommandsMenu.AppendMenu(wx.ID_ANY, cat, categories[cat])
                currentMenu = categories[cat]
            currentMenu.AppendMenuItem(cmd['title'], self, exec_user_cmd(cmd['cmd'], cmd['console'] if 'console' in cmd else None))
        menu.AppendMenu(wx.ID_ANY, title, userCommandsMenu)


    def ExecErlangCmd(self, cmd, consoleType = None):
        path = self.GetPyData(self.eventItem)

        console = self.project.shellConsole
        if consoleType == 'project':
            for title in self.project.consoleTabs:
                con = self.project.consoleTabs[title]
                if con.shell.stopped: continue
                console = con
                if core.ToolMgr.CurrentPage() == con:
                    break
        if IsModule(path):
            cmd = cmd.replace("$module$", self.project.ModuleName(path))
        cmd = cmd.replace("$application$", self.project.GetApp(path))
        cmd = cmd.replace("$file$", path)

        console.Exec(cmd + ".")

    def DefaultMask(self):
        return [".erl", ".hrl", ".config", ".c", ".cpp", ".bat", ".igor", ".src", ".app", ".html", ".xml", ".xhtml", ".css", '.js']

    def OnMenuNewHeader(self, event):
        result = self.RequestName("New Header", "Enter header name", "new_header")
        if not result: return
        (_, path) = result
        path = path + ".hrl"
        if path and not os.path.isfile(path):
            writeFile(path, "")
            core.TabMgr.LoadFileLine(path)
            self.dirChecker.CheckDirectoryChanges()

    def OnMenuNewApplication(self, event):
        result = self.RequestName("New Application", "Enter application name", "new_application")
        if not result: return
        (name, path) = result
        if not path: return
        if os.path.exists(path):
            wx.MessageBox("Folder {} exists.".format(path), "Error")
            return
        os.mkdir(path)
        srcPath = os.path.join(path, "src")
        os.mkdir(srcPath)
        os.mkdir(os.path.join(path, "include"))
        os.mkdir(os.path.join(path, "priv"))

        appName = name + "_app"
        supName = name + "_sup"
        appModulePath = os.path.join(srcPath, appName + ".erl")
        supModulePath = os.path.join(srcPath, supName + ".erl")
        appSrcPath = os.path.join(srcPath, name + ".app.src")

        app = self._GetTemplate("application.erl")
        app = app.replace("[module_name]", appName)
        app = app.replace("'TopSupervisor'", supName)

        sup = self._GetTemplate("supervisor.erl")
        sup = sup.replace("[module_name]", supName)

        appSrc = self._GetTemplate("app.src")
        appSrc = appSrc.replace("[app]", name)
        appSrc = appSrc.replace("[app_module_name]", appName)

        writeFile(appModulePath, app)
        writeFile(supModulePath, sup)
        writeFile(appSrcPath, appSrc)
        core.TabMgr.LoadFileLine(appModulePath)
        core.TabMgr.LoadFileLine(supModulePath)
        core.TabMgr.LoadFileLine(appSrcPath)
        self.dirChecker.CheckDirectoryChanges()


    def DefaultExcludeDirs(self):
        return ProjectExplorer.DefaultExcludeDirs(self) + [".settings"] #"ebin",

    def _GetTemplate(self, template):
        path = os.path.join(core.MainFrame.cwd, "data", "erlang", "templates", template)
        data = readFile(path)
        data = data.replace("[username]", Config.UserName())
        data = data.replace("[date]", time.strftime("%d.%m.%Y"))
        return data

    def CreateFromTemplate(self, template, title, defaultValue = "new_module_name", ext = ".erl", prompt = "Enter module name:", replaceWhat = "[module_name]"):
        result = self.RequestName(title, prompt, defaultValue)
        if not result: return
        (module, path) = result
        path = path + ext
        if path and not os.path.isfile(path):
            data = self._GetTemplate(template)
            data = data.replace(replaceWhat, module)
            writeFile(path, data)
            core.TabMgr.LoadFileLine(path)
            self.dirChecker.CheckDirectoryChanges()

    def CreateFromUserTemplate(self, template, title, defaultValue = "new_module_name", ext = ".erl", prompt = "Enter module name:", replaceWhat = "[module_name]"):
        self.CreateFromTemplate(os.path.join("user", template), title, defaultValue, ext, prompt, replaceWhat)

    def Rename(self, path):
        if os.path.isfile(path):
            what = "file"
        else:
            what = "dir"
        title = 'New ' + what + ' name:'
        dlg = ErlangRenameDialog(self, title, os.path.basename(path))
        if dlg.ShowModal() == wx.ID_OK:
            print("ok")
            newPath = os.path.join(os.path.dirname(path), dlg.GetPath())

            def updateEditor(old, new):
                page = core.TabMgr.FindPageIndexByPath(old)
                editor = core.TabMgr[page]
                editor.filePath = new
                editor.UpdateTabTitle()

            if os.path.isfile(path):
                if path in core.TabMgr.OpenedFiles():
                    if IsModule(path) and dlg.DoRenameModules():
                        (oldModuleName, ext) = os.path.splitext(os.path.basename(path))
                        (newModuleName, ext) = os.path.splitext(os.path.basename(newPath))
                        self.ReplaceOccurencesInProject(path, oldModuleName, newModuleName)

                    updateEditor(path, newPath)

            if os.path.isdir(path):
                for oPath in core.TabMgr.OpenedFiles():
                    if oPath.startswith(path):
                        oNewPath = oPath.replace(path, newPath)
                        updateEditor(oPath, oNewPath)

            os.rename(path, newPath)
            if IsModule(newPath) and os.path.basename(path) != os.path.basename(newPath):
                oldModuleName = os.path.basename(path)[:-4]
                newModuleName = os.path.basename(newPath)[:-4]
                self.ReplaceModuleName(newPath, oldModuleName, newModuleName)

        dlg.Destroy()

    def ReplaceOccurencesInProject(self, path, oldModuleName, newModuleName):
        self.ReplaceModuleName(path, oldModuleName, newModuleName)
        what = r"\b" + oldModuleName + ":"
        on = newModuleName + ":"
        ReplaceInProject(re.compile(what, re.MULTILINE | re.DOTALL), on, [".erl", ".hrl"])

    def ReplaceModuleName(self, path, oldModuleName, newModuleName):
        what = "-module\(" + oldModuleName + "\)"
        on = "-module(" + newModuleName + ")"
        ReplaceInFile(path, re.compile(what, re.MULTILINE | re.DOTALL), on)

    def AfterPasteMove(self, oldName, newName):
        if IsModule(newName) and os.path.basename(newName) != os.path.basename(oldName):
            oldModuleName = os.path.basename(oldName)[:-4]
            newModuleName = os.path.basename(newName)[:-4]
            self.ReplaceModuleName(newName, oldModuleName, newModuleName)

    def SetPathErrors(self, pathErrors):
        self.pathErrors = pathErrors
        self.needHighlight = True

    def OnHighlightTimer(self, event):
        if not self.needHighlight: return
        for (path, hasErrors) in self.pathErrors:
            item = self.FindItemByPath(path)
            if not item: continue
            color = ColorSchema.codeEditor["error_explorer_color"] if hasErrors else wx.NullColour
            while item:
                self.SetItemTextColour(item, color)
                item = self.GetItemParent(item)
        self.needHighlight = False

    def OnClose(self):
        ProjectExplorer.OnClose(self)
        self.highlightTimer.Stop()
import os
import re
import time
import wx
from idn_colorschema import ColorSchema
from idn_config import Config
from idn_findreplace import ReplaceInProject, ReplaceInFile
from idn_global import GetTabMgr, GetMainFrame
from idn_projectexplorer import ProjectExplorer
from idn_utils import Menu, writeFile, readFile, extension

__author__ = 'IDNoise'

class ErlangProjectExplorer(ProjectExplorer):
    def __init__(self, parent, project):
        ProjectExplorer.__init__(self, parent, project)
        self.pathErrors = {}
        self.needHighlight = True

        self.highlightTimer = wx.Timer(self, wx.NewId())
        self.highlightTimer.Start(250)
        self.Bind(wx.EVT_TIMER, self.OnHighlightTimer, self.highlightTimer)

    def CreateMenu(self):
        menu = ProjectExplorer.CreateMenu(self)
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
                self.CreateFromTemplate("app.src", "App Src", "application_1", ".app.src", "Enter application name:"))
            newMenu.AppendMenu(wx.NewId(), "Template", tMenu)

        dialyzerMenu = Menu()
        dialyzerMenu.AppendMenuItem("Project", self, lambda e: self.project.DialyzeProject())
        if self.eventItem == self.GetRootItem():
            menu.AppendSeparator()
            menu.AppendMenu(wx.NewId(), "Dialyzer", dialyzerMenu)
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
            menu.AppendMenu(wx.NewId(), "Dialyzer", dialyzerMenu)


        return menu

    def DefaultMask(self):
        return [".erl", ".hrl", ".config", ".c", ".cpp", ".bat", ".igor", ".src", ".app", ".html", ".xml", ".xhtml", ".css", '.js']

    def OnMenuNewHeader(self, event):
        (_, path) = self.RequestName("New Header", "Enter header name", "new_header")
        path = path + ".hrl"
        if path and not os.path.isfile(path):
            writeFile(path, "")
            GetTabMgr().LoadFileLine(path)

    def OnMenuNewApplication(self, event):
        (name, path) = self.RequestName("New Application", "Enter application name", "new_application")
        if not path or os.path.exists(path):
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
        appSrc = appSrc.replace("[module_name]", name)

        writeFile(appModulePath, app)
        writeFile(supModulePath, sup)
        writeFile(appSrcPath, appSrc)
        GetTabMgr().LoadFileLine(appModulePath)
        GetTabMgr().LoadFileLine(supModulePath)
        GetTabMgr().LoadFileLine(appSrcPath)


    def DefaultExcludeDirs(self):
        return ProjectExplorer.DefaultExcludeDirs(self) + [".settings"] #"ebin",

    def _GetTemplate(self, template):
        path = os.path.join(GetMainFrame().cwd, "data", "erlang", "templates", template)
        data = readFile(path)
        data = data.replace("[username]", Config.UserName())
        data = data.replace("[date]", time.strftime("%d.%m.%Y"))
        return data

    def CreateFromTemplate(self, template, title, defaultValue = "new_module_name", ext = ".erl", prompt = "Enter module name:"):
        (module, path) = self.RequestName(title, prompt, defaultValue)
        path = path + ext
        if path and not os.path.isfile(path):
            data = self._GetTemplate(template)
            data = data.replace("[module_name]", module)
            writeFile(path, data)
            GetTabMgr().LoadFileLine(path)

    def Rename(self, path):
        if os.path.isfile(path):
            what = "file"
        else:
            what = "dir"
        title = 'New ' + what + ' name:'
        dlg = wx.TextEntryDialog(self, title + ':', title,
            style = wx.OK | wx.CANCEL)
        dlg.SetValue(os.path.basename(path))
        if dlg.ShowModal() == wx.ID_OK:
            newPath = os.path.join(os.path.dirname(path), dlg.Value)

            def updateEditor(old, new):
                page = GetTabMgr().FindPageIndexByPath(old)
                editor = GetTabMgr()[page]
                editor.filePath = new
                editor.UpdateTabTitle()

            if os.path.isfile(path):
                if path in GetTabMgr().OpenedFiles():
                    if path.endswith(".erl"):
                        (oldModuleName, ext) = os.path.splitext(os.path.basename(path))
                        (newModuleName, ext) = os.path.splitext(os.path.basename(newPath))
                        self.ReplaceOccurencesInProject(path, oldModuleName, newModuleName)

                    updateEditor(path, newPath)

            if os.path.isdir(path):
                for oPath in GetTabMgr().OpenedFiles():
                    if oPath.startswith(path):
                        oNewPath = oPath.replace(path, newPath)
                        updateEditor(oPath, oNewPath)

            os.rename(path, newPath)
            if extension(newPath) == ".erl" and os.path.basename(path) != os.path.basename(newPath):
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
        if extension(newName) == ".erl" and os.path.basename(newName) != os.path.basename(oldName):
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
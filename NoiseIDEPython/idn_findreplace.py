from idn_window_utils import IDNCustomTreeCtrl

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
import re
from wx import stc
import wx.lib.agw.customtreectrl as CT
import core
from idn_utils import CreateButton, extension, writeFile, readFile, CreateBitmapButton, GetAllFilesInDir

class FindInFilePanel(wx.Panel):
    def __init__(self, parent, editor):
        wx.Panel.__init__(self, parent, size = (2000, 25))
        self.SetMaxSize((3000, 25))

        self.editor = editor
        self.editor.findPanel = self

        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        self.findText = wx.ComboBox(self, size = (200, 25))
        self.replaceText = wx.ComboBox(self, size = (200, 25))
        self.findButton = CreateButton(self, "Find", self.OnFind, wx.BU_EXACTFIT)
        self.replaceButton = CreateButton(self, "Replace", self.OnReplace, wx.BU_EXACTFIT)
        self.replaceAllButton = CreateButton(self, "Replace All", self.OnReplaceAll, wx.BU_EXACTFIT)
        self.closeButton = CreateBitmapButton(self, 'close.png', self.OnClose)
        self.closeButton.SetMaxSize((25, 25))
        self.searchUpCb = wx.CheckBox(self, label = "Search up")
        self.wholeWordsCb = wx.CheckBox(self, label = "Whole words")
        self.matchCaseCb = wx.CheckBox(self, label = "Match case")
        self.useRegextCb = wx.CheckBox(self, label = "Regexp")

        self.sizer.Add(self.findText, 0, wx.ALL, 2)
        self.sizer.Add(self.findButton)
        self.sizer.AddSpacer(5)
        self.sizer.Add(self.searchUpCb, flag = wx.ALIGN_CENTER_VERTICAL)
        self.sizer.Add(self.wholeWordsCb, flag = wx.ALIGN_CENTER_VERTICAL)
        self.sizer.Add(self.matchCaseCb, flag = wx.ALIGN_CENTER_VERTICAL)
        self.sizer.Add(self.useRegextCb, flag = wx.ALIGN_CENTER_VERTICAL)
        self.sizer.AddSpacer(5)
        self.sizer.Add(self.replaceText, 0, wx.ALL, 2)
        self.sizer.Add(self.replaceButton)
        self.sizer.Add(self.replaceAllButton)
        self.sizer.AddStretchSpacer()
        self.sizer.Add(self.closeButton, 0, flag = wx.ALIGN_RIGHT)

        self.SetSizer(self.sizer)
        self.Layout()

        self.findText.Bind(wx.EVT_KEY_UP, self.OnFindKeyUp)

        self.incremental = False

    def OnFindKeyUp(self, event):
        event.Skip()
        keyCode = event.GetKeyCode()
        if keyCode == wx.WXK_RETURN and self.IsShown:
            self.OnFind()

        if self.incremental and self.findText.Value:
            self.OnFind()
            if self.findSuccessful:
                anchor = self.editor.GetAnchor()
                pos = self.editor.CurrentPos
                if self.searchUpCb.Value:
                    self.editor.GotoPos(pos)
                    self.editor.SetAnchor(anchor)
                else:
                    self.editor.GotoPos(anchor)
                    self.editor.SetAnchor(pos)
        if self.TryFind() or not self.findText.Value:
            self.findText.SetBackgroundColour(wx.WHITE)
        else:
            self.findText.SetBackgroundColour(wx.Colour(200, 80, 80))
        self.findText.SetForegroundColour(wx.BLACK)
        self.findText.Refresh()
        self.findText.ClearBackground()

    def OnFind(self, event = None):
        self.textToFind = self.findText.Value
        if self.textToFind and self.editor:
            if not self.textToFind in self.findText.Items:
                self.findText.Append(self.textToFind)
        else:
            return
        wholeWords = self.wholeWordsCb.Value
        matchCase = self.matchCaseCb.Value
        useRegexp = self.useRegextCb.Value
        searchDown = not self.searchUpCb.Value
        self.findSuccessful = self.FindText(self.textToFind, wholeWords, matchCase, useRegexp, searchDown)

    def FindText(self, text, wholeWords = True, matchCase = True, useRegexp = False, searchDown = True, select = True):
        findOptions = 0
        if useRegexp:
            findOptions |= stc.STC_FIND_REGEXP
        else:
            if wholeWords:
                findOptions |= stc.STC_FIND_WHOLEWORD
            if matchCase:
                findOptions |= stc.STC_FIND_MATCHCASE
        startPos = self.editor.CurrentPos
        for attempt in range(2):
            caretPosition = self.editor.CurrentPos
            self.editor.SetAnchor(caretPosition)
            self.editor.SearchAnchor()
            def search():
                if searchDown:
                    return self.editor.SearchNext(findOptions, text)
                else:
                    return self.editor.SearchPrev(findOptions, text)
            pos = search()
            if pos == startPos:
                caretPosition += 1 if searchDown else -1
                self.editor.SetCurrentPos(caretPosition)
                self.editor.SearchAnchor()
                pos = search()
            if pos >= 0:
                if select:
                    self.editor.GotoPos(pos)
                    self.editor.SetAnchor(pos + len(text))
                else:
                    self.editor.GotoPos(pos)
                    self.editor.SetAnchor(pos)
                return True
            else:
                if attempt == 0:
                    if searchDown:
                        self.editor.SetCurrentPos(0)
                    else:
                        self.editor.SetCurrentPos(self.editor.Length)
                    continue
                else:
                    self.editor.SetCurrentPos(startPos)
                    self.editor.SetAnchor(startPos)
                    break
        return False

    def TryFind(self):
        findOptions = 0
        wholeWords = self.wholeWordsCb.Value
        matchCase = self.matchCaseCb.Value
        useRegexp = self.useRegextCb.Value
        if useRegexp:
            findOptions |= stc.STC_FIND_REGEXP
        else:
            if wholeWords:
                findOptions |= stc.STC_FIND_WHOLEWORD
            if matchCase:
                findOptions |= stc.STC_FIND_MATCHCASE


        oldFlags = self.editor.GetSearchFlags()
        self.editor.SetSearchFlags(findOptions)
        self.editor.SetTargetStart(0)
        self.editor.SetTargetEnd(self.editor.Length)
        pos = self.editor.SearchInTarget(self.findText.Value)
        self.editor.SetSearchFlags(oldFlags)
        return pos >= 0

    def OnReplace(self, event):
        replaceText = self.replaceText.Value
        if self.editor and self.findText.Value == self.textToFind:
            if replaceText and not replaceText in self.replaceText.Items:
                self.replaceText.Append(replaceText)

            self.editor.ReplaceSelection(self.replaceText.Value)
            self.OnFind(None)

    def OnReplaceAll(self, event):
        if not self.findText.Value:
            return

        regexp = PrepareRegexp(self.findText.Value,
                               self.wholeWordsCb.Value,
                               self.matchCaseCb.Value,
                               self.useRegextCb.Value)
        text = self.editor.GetText()
        if regexp.search(text):
            text = regexp.sub(self.replaceText.Value, text)
            self.editor.SetText(text)

    def OnClose(self, event):
        self.Parent.HideFind()

class FindInProjectDialog(wx.Dialog):
    resultsTable = None
    dialog = None

    @classmethod
    def GetDialog(cls, parent = None):
        if not FindInProjectDialog.dialog:
            FindInProjectDialog.dialog = FindInProjectDialog(parent)
        return FindInProjectDialog.dialog

    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, id = wx.ID_ANY, title = "Find / Replace in project")
        wx.ToolTip_Enable(True)
        self.findText = wx.ComboBox(self, size = (300, 25))
        self.replaceText = wx.ComboBox(self, size = (300, 25))
        self.findButton = CreateButton(self, "Find", self.OnFind)
        self.replaceButton = CreateButton(self, "Replace", self.OnReplace)

        self.fileExtensionTB = wx.TextCtrl(self, size = (300, 25), value = "*.erl; *.hrl")
        self.fileExtensionTB.SetToolTipString("Semicolon separated, spaces ignored. Example: '*.erl;*.hrl'")

        self.searchDirTB = wx.TextCtrl(self, size = (300, 25))
        self.searchDirTB.SetToolTipString("Relative to project dir: {}. Example: 'apps'".format(core.Project.projectDir))
        self.searchDirButton = CreateButton(self, "...", self.OnSelectProjectPath)
        self.searchDirButton.MinSize = (25, 25)

        self.wholeWordsCb = wx.CheckBox(self, label = "Whole words")
        self.matchCaseCb = wx.CheckBox(self, label = "Match case")
        self.useRegextCb = wx.CheckBox(self, label = "Regexp")
        self.openNewSearchResultCb = wx.CheckBox(self, label = "Open new tab for result")

        self.sizer = wx.GridBagSizer(1, 1)
        i = 0
        self.sizer.Add(wx.StaticText(self, label = "Find text:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 10)
        self.sizer.Add(self.findText, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.findButton, (i, 2), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        i += 1
        self.sizer.Add(wx.StaticText(self, label = "Replace text:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 10)
        self.sizer.Add(self.replaceText, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.replaceButton, (i, 2), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        i += 1
        self.sizer.Add(wx.StaticText(self, label = "File extensions:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 10)
        self.sizer.Add(self.fileExtensionTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        i += 1
        self.sizer.Add(wx.StaticText(self, label = "Search dir:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 10)
        self.sizer.Add(self.searchDirTB, (i, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.searchDirButton, (i, 2), flag = wx.ALL | wx.ALIGN_LEFT, border = 2)
        i += 1
        self.sizer.Add(self.useRegextCb, (i, 1), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        i += 1
        self.sizer.Add(self.wholeWordsCb, (i, 1), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        i += 1
        self.sizer.Add(self.matchCaseCb, (i, 1), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        i += 1
        self.sizer.Add(self.openNewSearchResultCb, (i, 1), flag = wx.LEFT | wx.BOTTOM | wx.ALIGN_CENTER_VERTICAL, border = 10)
        self.SetSizer(self.sizer)
        self.Layout()
        self.sizer.SetSizeHints(self)

        self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

    def OnSelectProjectPath(self, event):
        dlg = wx.DirDialog(self, defaultPath = core.Project.projectDir)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            if not path.startswith(core.Project.projectDir):
                return
            path = path.replace(core.Project.projectDir + os.sep, "")
            self.searchDirTB.Value = path

    def OnFind(self, event = None):
        textToFind = self.findText.Value
        if not textToFind: return
        fileExts = []
        if self.fileExtensionTB.Value:
            value = self.fileExtensionTB.Value.replace(" ", "").replace("*", "")
            fileExts = value.split(";")

        searchDir = core.Project.projectDir
        path = os.path.join(searchDir, self.searchDirTB.Value)
        if os.path.isdir(path):
            searchDir = path

        Find(textToFind,
             "Find Results: {}".format(textToFind),
             wholeWords = self.wholeWordsCb.Value,
             matchCase = self.matchCaseCb.Value,
             useRegexp = self.useRegextCb.Value,
             openNewTab = self.openNewSearchResultCb.Value,
             fileExts = fileExts,
             searchDir = searchDir
            )

        if not textToFind and not textToFind in self.findText.Items:
            self.findText.Append(textToFind)

    def OnReplace(self, event = None):
        textToFind = self.findText.Value
        if not textToFind:
            return
        if not textToFind in self.findText.Items:
            self.findText.Append(textToFind)

        self.textToReplace = self.replaceText.Value
        if not self.textToReplace:
            return

        if not self.textToReplace in self.replaceText.Items:
            self.replaceText.Append(self.textToReplace)

        regexp = PrepareRegexp(textToFind,
                               self.wholeWordsCb.Value,
                               self.matchCaseCb.Value,
                               self.useRegextCb.Value)
        ReplaceInProject(regexp, self.textToReplace)


    def OnKeyDown(self, event):
        keyCode = event.GetKeyCode()
        if keyCode == wx.WXK_ESCAPE:
            self.Hide()
        elif keyCode == wx.WXK_RETURN:
            self.OnFind()
        else:
            event.Skip()

def Find(textToFind = "", title = "Find results", wholeWords = False, matchCase = False, useRegexp = False, openNewTab = True, fileExts = None, searchDir = None, resultsFilter = None, showResult = True):
    if not textToFind:
        return

    if not fileExts:
        fileExts = []
    if not searchDir or not os.path.isdir(searchDir):
        searchDir = core.Project.projectDir

    core.MainFrame.SetCursor(wx.StockCursor(wx.CURSOR_WAIT))
    try:
        results = {}
        regexp = PrepareRegexp(textToFind, wholeWords, matchCase, useRegexp)
        filePaths = GetAllFilesInDir(searchDir, fileExts)
        for filePath in filePaths:
            result = SearchInFile(filePath, regexp)
            if result:
                if resultsFilter:
                    for res in result[:]:
                        if not resultsFilter(res):
                            result.remove(res)
                if result:
                    results[filePath] = result
        if showResult:
            FillFindResultsTable(results, len(filePaths), regexp, openNewTab, title, searchDir, fileExts, resultsFilter)
    finally:
        core.MainFrame.SetCursor(wx.StockCursor(wx.CURSOR_DEFAULT))
    return results;


def SearchInFile(filePath, regexp):
    try:
        result = []
        lineNumber = 0
        if filePath in core.TabMgr.OpenedFiles():
            fileText = core.TabMgr.FindPageByPath(filePath).GetText().split("\n")
        else:
            f = open(filePath, "r")
            fileText = f.readlines()
            f.close()
        for lineText in fileText:
            end = 0
            while True:
                m = regexp.search(lineText, end)
                if not m: break
                start = m.start()
                end = m.end()
                result.append(SearchResult(filePath, lineNumber, lineText, start, end))
            lineNumber += 1
        return result
    except Exception, e:
        core.Log("find in project error", e)

def PrepareRegexp(textToFind, wholeWords = False, matchCase = False, useRegexp = False):
    flags = re.MULTILINE | re.DOTALL
    pattern = textToFind
    if not useRegexp:
        pattern = re.escape(pattern)
    if wholeWords:
        pattern = r"\b" + pattern + r"\b"
    if not matchCase:
        flags |= re.IGNORECASE
    return re.compile(pattern, flags)

def FillFindResultsTable(results, filesCount, regexp, openNewTab, title, searchDir, fileExts, resultsFilter):
    resultsTable = None
    if not openNewTab:
        for page in reversed(core.ToolMgr.Pages()):
            if isinstance(page, FindResultsTree):
                id = core.ToolMgr.FindPageIndexByWindow(page)
                resultsTable = page
                core.ToolMgr.SetPageText(id, title)
                break
    if not resultsTable:
        resultsTable = FindResultsTree(core.ToolMgr)
        core.ToolMgr.AddPage(resultsTable, title, True)
    resultsTable.SetResults(results, filesCount, regexp, searchDir, fileExts, resultsFilter)
    core.ToolMgr.FocusOnWidget(resultsTable)


def ReplaceInProject(regexp, replacement, mask = None):
    filePaths = core.Project.explorer.GetAllFiles()
    for filePath in sorted(filePaths):
        if mask and extension(filePath) not in mask:
            continue
        ReplaceInFile(filePath, regexp, replacement)

def ReplaceInFile(filePath, regexp, replacement):
    try:
        if filePath in core.TabMgr.OpenedFiles():
            editor = core.TabMgr.FindPageByPath(filePath)
            text = editor.GetText()
            if regexp.search(text):
                text = regexp.sub(replacement, text)
                editor.SetText(text)
                editor.Save()
        else:
            fileText = readFile(filePath)
            if regexp.search(fileText):
                fileText = regexp.sub(replacement, fileText)
                writeFile(filePath, fileText)
    except Exception, e:
        core.Log("replace in project error: '", filePath, e)

class FindResultsTree(IDNCustomTreeCtrl):
    def __init__(self, parent):
        IDNCustomTreeCtrl.__init__(self, parent)
        self.results = []
        self.filesCount = 0
        self.searchDir = core.Project.projectDir
        self.fileExts = []
        self.resultsFilter = None
        self.regexp = None
        self.Bind(CT.EVT_TREE_ITEM_ACTIVATED, self.OnActivateItem)
        core.Project.explorer.ProjectFilesCreatedEvent += self.OnProjectFilesCreated
        core.Project.explorer.ProjectFilesModifiedEvent += self.OnProjectFilesModified
        core.Project.explorer.ProjectFilesDeletedEvent += self.OnProjectFilesDeleted
        self.Bind(wx.EVT_WINDOW_DESTROY, self.OnDestroy)

    def OnDestroy(self, event):
        core.Project.explorer.ProjectFilesCreatedEvent -= self.OnProjectFilesCreated
        core.Project.explorer.ProjectFilesModifiedEvent -= self.OnProjectFilesModified
        core.Project.explorer.ProjectFilesDeletedEvent -= self.OnProjectFilesDeleted

    def OnProjectFilesCreated(self, filePaths):
        for filePath in filePaths:
            if (self.regexp and filePath.startswith(self.searchDir) and
                (self.fileExts and any([filePath.endswith(fm) for fm in self.fileExts]))):
                result = SearchInFile(filePath, self.regexp)
                for r in result[:]:
                    if self.resultsFilter and not self.resultsFilter(r):
                        result.remove(r)
                self.results[filePath] = result
                self.UpdateResults()

    def OnProjectFilesModified(self, filePaths):
        for filePath in filePaths:
            if self.regexp and filePath in self.results:
                result = SearchInFile(filePath, self.regexp)
                self.results[filePath] = result
                wx.CallAfter(self.UpdateResults)

    def OnProjectFilesDeleted(self, filePaths):
        for filePath in filePaths:
            if self.regexp and filePath in self.results:
                self.results[filePath] = None
                wx.CallAfter(self.UpdateResults)

    def UpdateResults(self):
        expanded = []
        for node in self.GetItemChildren(self.GetRootItem()):
            if self.IsExpanded(node):
                expanded.append(self.GetPyData(node).file)

        self.SetResults(self.results, self.filesCount, self.regexp, self.searchDir, self.fileExts, self.resultsFilter)
        for node in self.GetItemChildren(self.GetRootItem()):
            if self.GetPyData(node).file in expanded:
                self.Expand(node)

    def CleanUp(self):
        self.results = []
        self.DeleteAllItems()

    def SetResults(self, results, filesCount, regexp, searchDir, fileExts, resultsFilter):
        self.results = results
        self.filesCount = filesCount
        self.regexp = regexp
        self.searchDir = searchDir
        self.fileExts = fileExts
        self.resultsFilter = resultsFilter
        self.DeleteAllItems()
        rootNode = self.AddRoot("0 results in {0} files".format(filesCount))
        self.SetPyData(rootNode, FindResultsTreeItemPyData())
        if not results:
            return

        self.SetItemHasChildren(rootNode, True)
        resultsCount = 0

        for (filePath, res) in results.items():
            if not res or len(res) == 0:
                continue
            resultsCount += len(res)
            fileLabel = filePath.replace(core.Project.projectDir + os.sep, "")
            fileNode = self.AppendItem(rootNode, "{0}: {1} results".format(fileLabel, len(res)))

            self.SetPyData(fileNode, FindResultsTreeItemPyData(filePath))
            self.SetItemHasChildren(fileNode, True)
            for result in res:
                resultNode = self.AppendItem(fileNode,
                    '{0:{fill}{align}14} {1}'.format('Line: ' + str(result.lineNumber + 1),
                        result.lineText.replace("\n", "").strip(), fill=" ", align="<"))
                self.SetPyData(resultNode,
                    FindResultsTreeItemPyData(filePath, result.lineNumber, result.start, result.end))

        self.SetItemText(rootNode, "{0} results in {1} files".format(resultsCount, filesCount))
        self.Expand(rootNode)
        self.SortChildren(rootNode)

    def OnActivateItem(self, event):
        data = self.GetPyData(event.GetItem())
        if not data.file: return
        editor = core.TabMgr.LoadFileLine(data.file, data.lineNumber)
        if data.lineNumber:
            editor.GotoLine(data.lineNumber)
            pos = editor.PositionFromLine(data.lineNumber)
            editor.SetSelection(pos + data.start, pos + data.end)

class FindResultsTreeItemPyData:
    def __init__(self, file = None, lineNumber = 0, start = None, end = None):
        self.file = file
        self.lineNumber = lineNumber
        self.start = start
        self.end = end

class SearchResult:
    def __init__(self, file, lineNumber, lineText, start, end):
        self.file = file
        self.lineNumber = lineNumber
        self.lineText = lineText
        self.start = start
        self.end = end

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
import re
from wx import stc
import wx.lib.agw.customtreectrl as CT
from idn_global import GetTabMgr, GetProject, GetToolMgr, Log
from idn_utils import CreateButton, extension, writeFile, readFile, CreateBitmapButton

class FindInFilePanel(wx.Panel):
    def __init__(self, parent, editor):
        wx.Panel.__init__(self, parent, size = (2000, 25))
        self.SetMaxSize((3000, 25))

        self.editor = editor

        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        self.findText = wx.ComboBox(self, size = (200, 25))
        self.replaceText = wx.ComboBox(self, size = (200, 25))
        self.findButton = CreateButton(self, "Find", self.OnFind, wx.BU_EXACTFIT)
        self.replaceButton = CreateButton(self, "Replace", self.OnReplace, wx.BU_EXACTFIT)
        self.replaceAllButton = CreateButton(self, "Replace All", self.OnReplaceAll, wx.BU_EXACTFIT)
        self.closeButton = CreateBitmapButton(self, 'clear_console.png', self.OnClose)
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

        #self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

    def OnFind(self, event):
        self.textToFind = self.findText.Value
        self.findSuccessful = False
        if self.textToFind and self.editor:
            if not self.textToFind in self.findText.Items:
                self.findText.Append(self.textToFind)

            findOptions = 0
            wholeWords = self.wholeWordsCb.Value
            matchCase = self.matchCaseCb.Value
            searchDown = not self.searchUpCb.Value
            useRegexp = self.useRegextCb.Value
            if useRegexp:
                findOptions |= stc.STC_FIND_REGEXP
            else:
                if wholeWords:
                    findOptions |= stc.STC_FIND_WHOLEWORD
                if matchCase:
                    findOptions |= stc.STC_FIND_MATCHCASE

            for attempt in range(2):
                caretPosition = self.editor.CurrentPos
                self.editor.SetAnchor(caretPosition)
                self.editor.SearchAnchor()
                if searchDown:
                    pos = self.editor.SearchNext(findOptions, self.textToFind)
                else:
                    pos = self.editor.SearchPrev(findOptions, self.textToFind)
                if pos >= 0:
                    if searchDown:
                        self.editor.GotoPos(pos + len(self.textToFind))
                        self.editor.SetAnchor(pos)
                    else:
                        self.editor.GotoPos(pos)
                        self.editor.SetAnchor(pos + len(self.textToFind))
                    self.findSuccessful = True
                    break
                else:
                    if attempt == 0:
                        if searchDown:
                            self.editor.SetCurrentPos(0)
                        else:
                            self.editor.SetCurrentPos(self.editor.Length)
                        continue
                    else:
                        self.editor.SetCurrentPos(caretPosition)
                        break

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
        self.OnFind(None)
        while self.findSuccessful:
            self.OnReplace(None)
            self.OnFind(None)

    def OnClose(self, event):
        self.Parent.HideFind()

    def OnKeyDown(self, event):
        keyCode = event.GetKeyCode()
        if keyCode == wx.WXK_ESCAPE:
            self.Parent.HideFind()
        else:
            event.Skip()

class FindInProjectDialog(wx.Dialog):
    resultsTable = None

    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, id = wx.NewId(), title = "Find / Replace in project", size = (520, 195))

        self.sizer = wx.GridBagSizer(2, 2)
        self.findTextLabel = wx.StaticText(self, label = "Find text:")
        self.findText = wx.ComboBox(self, size = (300, 25))
        self.replaceTextLabel = wx.StaticText(self, label = "Replace text:")
        self.replaceText = wx.ComboBox(self, size = (300, 25))
        self.findButton = CreateButton(self, "Find", self.OnFind)
        self.replaceButton = CreateButton(self, "Replace", self.OnReplace)

        self.wholeWordsCb = wx.CheckBox(self, label = "Whole words")
        self.matchCaseCb = wx.CheckBox(self, label = "Match case")
        self.useRegextCb = wx.CheckBox(self, label = "Regexp")
        self.sizer.Add(self.findTextLabel, (0, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.findText, (0, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.findButton, (0, 2), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.replaceTextLabel, (1, 0), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.replaceText, (1, 1), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.replaceButton, (1, 2), flag = wx.ALL | wx.ALIGN_CENTER, border = 2)
        self.sizer.Add(self.useRegextCb, (2, 0), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        self.sizer.Add(self.wholeWordsCb, (3, 0), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        self.sizer.Add(self.matchCaseCb, (4, 0), flag = wx.LEFT | wx.ALIGN_CENTER_VERTICAL, border = 10)
        self.SetSizer(self.sizer)
        self.Layout()

        self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyDown)

    def OnFind(self, event):
        self.textToFind = self.findText.Value
        if not self.textToFind:
            return

        results = []
        regexp = self.PrepareRegexp()
        files = GetProject().explorer.GetAllFiles()
        for file in sorted(files):
            try:
                result = []
                lineNumber = 0
                if file in GetTabMgr().OpenedFiles():
                    fileText = GetTabMgr().FindPageByPath(file).GetText().split("\n")
                else:
                    fileText = open(file, "r")
                for lineText in fileText:
                    end = 0
                    while True:
                        m = regexp.search(lineText, end)
                        if not m: break
                        start = m.start()
                        end = m.end()
                        result.append(SearchResult(file, lineNumber, lineText, start, end))
                    lineNumber += 1
                if result:
                    results.append((file, result))
            except Exception, e:
                Log("find in project error", e)
                continue
        self.FillFindResultsTable(results, len(files))

    def OnReplace(self, event):
        self.textToFind = self.findText.Value
        if not self.textToFind:
            return

        self.textToReplace = self.replaceText.Value
        if not self.textToReplace:
            return

        regexp = self.PrepareRegexp()
        ReplaceInProject(regexp, self.textToReplace)


    def PrepareRegexp(self):
        wholeWords = self.wholeWordsCb.Value
        matchCase = self.matchCaseCb.Value
        useRegexp = self.useRegextCb.Value

        flags = re.MULTILINE | re.DOTALL
        pattern = self.textToFind
        if not useRegexp:
            pattern = re.escape(pattern)
        if wholeWords:
            pattern = r"\b" + pattern + r"\b"
        if not matchCase:
            flags |= re.IGNORECASE
        #print "search", pattern
        return re.compile(pattern, flags)

    def FillFindResultsTable(self, results, filesCount):
        if not FindInProjectDialog.resultsTable:
            FindInProjectDialog.resultsTable = ErrorsTree(GetToolMgr())
            GetToolMgr().AddPage(self.resultsTable, "Find Results")
        GetToolMgr().SetSelection(GetToolMgr().GetPageIndex(FindInProjectDialog.resultsTable))
        FindInProjectDialog.resultsTable.SetResults(results, filesCount)

    def OnKeyDown(self, event):
        keyCode = event.GetKeyCode()
        if keyCode == wx.WXK_ESCAPE:
            self.Parent.sizer.Show(self.Parent.findPanel, False)
            self.Parent.Layout()
        else:
            event.Skip()

def ReplaceInProject(regexp, replacement, mask = None):
    files = GetProject().explorer.GetAllFiles()
    for file in sorted(files):
        if mask and extension(file) not in mask:
            continue
        ReplaceInFile(file, regexp, replacement)

def ReplaceInFile(file, regexp, replacement):
    try:
        if file in GetTabMgr().OpenedFiles():
            editor = GetTabMgr().FindPageByPath(file)
            text = editor.GetText()
            if regexp.search(text):
                text = regexp.sub(replacement, text)
                editor.SetText(text)
                editor.Save()
        else:
            fileText = readFile(file)
            if regexp.search(fileText):
                fileText = regexp.sub(replacement, fileText)
                writeFile(file, fileText)
    except Exception, e:
        Log("replace in project error: '", file, e)

class ErrorsTree(CT.CustomTreeCtrl):
    def __init__(self, parent):
        CT.CustomTreeCtrl.__init__(self, parent)
        self.Bind(CT.EVT_TREE_ITEM_ACTIVATED, self.OnActivateItem)

    def SetResults(self, results, filesCount):
        self.DeleteAllItems()
        rootNode = self.AddRoot("0 results in {0} files".format(filesCount))
        self.SetPyData(rootNode, ErrorsTreeItemPyData())
        if not results:
            return

        self.SetItemHasChildren(rootNode, True)
        resultsCount = 0
        for (file, res) in results:
            resultsCount += len(res)
            fileLabel = file.replace(GetProject().projectDir + os.sep, "")
            fileNode = self.AppendItem(rootNode, "{0}: {1} results".format(fileLabel, len(res)))
            self.SetPyData(fileNode, ErrorsTreeItemPyData(file))
            self.SetItemHasChildren(fileNode, True)
            for result in res:
                resultNode = self.AppendItem(fileNode,
                    '{0:{fill}{align}14} {1}'.format('Line: ' + str(result.lineNumber + 1),
                        result.lineText.replace("\n", "").strip(), fill=" ", align="<"))
                self.SetPyData(resultNode,
                    ErrorsTreeItemPyData(file, result.lineNumber, result.start, result.end))
        self.SetItemText(rootNode, "{0} results in {1} files".format(resultsCount, filesCount))
        self.SetFocus()
        self.Expand(rootNode)

    def OnActivateItem(self, event):
        data = self.GetPyData(event.GetItem())
        if not data.file: return
        editor = GetTabMgr().LoadFile(data.file)
        if data.lineNumber:
            editor.GotoLine(data.lineNumber)
            pos = editor.PositionFromLine(data.lineNumber)
            #Log(data.lineNumber, pos, data.start, data.end)
           # editor.SetSelection(pos, pos + 1)
            editor.SetSelection(pos + data.start, pos + data.end)

class ErrorsTreeItemPyData:
    def __init__(self, file = None, lineNumber = None, start = None, end = None):
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
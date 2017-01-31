from exceptions import IndexError
import operator
import os
import wx
from wx.grid import PyGridTableBase
from idn_cache import ErlangCache
import core

__author__ = 'Yaroslav'

class ErrorsTableGrid(wx.grid.Grid):
    def __init__(self, parent, project):
        wx.grid.Grid.__init__(self, parent, -1)
        self.project = project
        self.table = self.CreateTable()
        self.SetTable(self.table, True)
        self.AutoSizeColumns(False)
        self.SetRowLabelSize(25)
        self.SetMargins(0,0)
        self.SetColSizes()
        self.EnableEditing(False)
        self.SetColMinimalAcceptableWidth(40)
        self.SetRowMinimalAcceptableHeight(10)

        self.DisableCellEditControl()
        self.DisableDragCell()
        self.DisableDragColMove()
        self.DisableDragGridSize()

        self.SetDefaultRenderer(CutomGridCellAutoWrapStringRenderer())

        self.Bind(wx.grid.EVT_GRID_CELL_LEFT_DCLICK, self.OnLeftDClick)
        self.pathDict = {}

    def SetColSizes(self):
        self.SetColSize(0, 350)
        self.SetColSize(1, 40)
        self.SetColSize(2, 70)
        self.SetColSize(3, 750)

    def CreateTable(self):
        return ErrorsTable([])

    def OnLeftDClick(self, event):
        row = event.GetRow()
        rowData = self.table.data[row]
        if not rowData[0]:
            return
        fileName = os.path.join(self.project.projectDir, rowData[0])
        line = rowData[1] - 1
        core.TabMgr.LoadFileLine(fileName, line)

    def AddErrors(self, path, errors):
        currentRows = len(self.table.data)
        newPath = path.replace(self.project.projectDir + os.sep, "")
        self.pathDict[newPath] = path
        data = list(filter(lambda x: x[0] != newPath, self.table.data))
        for e in errors:
            data.append((newPath, e.line + 1, e.TypeToStr(), e.msg))
        data = sorted(data, key = operator.itemgetter(2, 0))
        self.table.data = data
        self.table.ResetView(self, currentRows)


class ErrorsTable(PyGridTableBase):
    def __init__(self, data):
        PyGridTableBase.__init__(self)
        self.data = data

        self.colLabels = ["File", "Line", "Type", "Message"]

    def GetNumberRows(self):
        return len(self.data)

    def GetNumberCols(self):
         return len(self.colLabels)

    def IsEmptyCell(self, row, col):
        try:
            return not self.data[row][col]
        except IndexError:
            return True

    def GetValue(self, row, col):
        try:
            return self.data[row][col]
        except IndexError:
            return ''

    def GetTypeName(self, row, col):
        if col == 1:
            return wx.grid.GRID_VALUE_NUMBER
        else:
            return wx.grid.GRID_VALUE_STRING

    def GetColLabelValue(self, col):
         return self.colLabels[col]

    def SetValue(self, row, col, value):
        self.data[row][col] = value

    def ResetView(self, grid, currentRows):
        """
        (Grid) -> Reset the grid view.   Call this to
        update the grid if rows and columns have been added or deleted
        """
        grid.BeginBatch()

        for current, new, delmsg, addmsg in [
            (currentRows, self.GetNumberRows(), wx.grid.GRIDTABLE_NOTIFY_ROWS_DELETED, wx.grid.GRIDTABLE_NOTIFY_ROWS_APPENDED)
        ]:

            if new < current:
                msg = wx.grid.GridTableMessage(self,delmsg,new,current-new)
                grid.ProcessTableMessage(msg)
            elif new > current:
                msg = wx.grid.GridTableMessage(self,addmsg,new-current)
                grid.ProcessTableMessage(msg)
            self.UpdateValues(grid)

        grid.EndBatch()

        # update the scrollbars and the displayed part of the grid
        grid.AdjustScrollbars()
        grid.ForceRefresh()


    def UpdateValues(self, grid):
        """Update all displayed values"""
        # This sends an event to the grid table to update all of the values
        msg = wx.grid.GridTableMessage(self, wx.grid.GRIDTABLE_REQUEST_VIEW_GET_VALUES)
        grid.ProcessTableMessage(msg)

class XrefTable(ErrorsTable):
    def __init__(self, data):
        ErrorsTable.__init__(self, data)
        self.colLabels = ["File", "Line", "Function", "Undefined function call"]

class XrefTableGrid(ErrorsTableGrid):
    def __init__(self, parent, project):
        ErrorsTableGrid.__init__(self, parent, project)
        self.pathErrors = {}

    def SetColSizes(self):
        self.SetColSize(0, 350)
        self.SetColSize(1, 40)
        self.SetColSize(2, 300)
        self.SetColSize(3, 300)

    def CreateTable(self):
        return XrefTable([])

    def Clear(self):
        current = len(self.table.data)
        self.table.data = []
        self.pathErrors = {}
        self.table.ResetView(self, current)

    def AddErrors(self, path, errors):
        if path in self.pathErrors and self.pathErrors[path] == errors:
            return
        if path not in self.pathErrors and not errors:
            return
        currentRows = len(self.table.data)
        newPath = path.replace(self.project.projectDir + os.sep, "")
        self.pathDict[newPath] = path
        data = list(filter(lambda x: x[0] != newPath, self.table.data))
        for ((wm, wf, wa), (m, f, a)) in errors:
            funData = ErlangCache.ModuleFunction(wm, wf, wa)
            if not funData:
                data.append((newPath, 0, "{}:{}/{}".format(wm, wf, wa), "{}:{}/{}".format(m, f, a)))
            else:
                data.append((newPath, funData.line, "{}:{}/{}".format(wm, wf, wa), "{}:{}/{}".format(m, f, a)))
        data = sorted(data, key = operator.itemgetter(0))
        self.table.data = data
        self.table.ResetView(self, currentRows)
        self.pathErrors[path] = errors

    def PrepareResult(self):
        self.Unbind(wx.grid.EVT_GRID_CELL_LEFT_DCLICK)
        if len(self.table.data) == 0:
            self.table.data = [("Project", 0, "No problems found")]
            self.table.ResetView(self, 0)
        else:
            self.Bind(wx.grid.EVT_GRID_CELL_LEFT_DCLICK, self.OnLeftDClick)

    def OnLeftDClick(self, event):
        row = event.GetRow()
        rowData = self.table.data[row]
        filePath = os.path.join(self.project.projectDir, rowData[0])
        line = rowData[1] - 1
        stc = core.TabMgr.LoadFileLine(filePath, line)
        fun, arity = rowData[3].split("/")
        stc.HighlightSelectedWord(fun, stc.PositionFromLine(line))

class DialyzerTable(ErrorsTable):
    def __init__(self, data):
        ErrorsTable.__init__(self, data)
        self.colLabels = ["File", "Line", "Data"]

class DialyzerTableGrid(ErrorsTableGrid):
    def __init__(self, parent, project):
        ErrorsTableGrid.__init__(self, parent, project)
        self.EnableDragColSize()
        self.EnableDragRowSize()
        self.EnableGridLines()

    def SetColSizes(self):
        self.SetColSize(0, 350)
        self.SetColSize(1, 40)
        self.SetColSize(2, 850)

    def CreateTable(self):
        return DialyzerTable([])

    def Clear(self):
        current = len(self.table.data)
        self.table.data = []
        self.table.ResetView(self, current)

    def SetWarnings(self, warnings):
        currentRows = len(self.table.data)
        data = []
        for warning in warnings:
            warningData = warning.split(":")
            module = warningData[0]
            line = int(warningData[1])
            msg = ':'.join(warningData[2:])
            if not module:
                data.append(('', line, msg))
            else:
                path = ErlangCache.modules[os.path.splitext(module)[0]].file
                newPath = path.replace(self.project.projectDir + os.sep, "")
                self.pathDict[newPath] = path
                data.append((newPath, line, msg))

        data = sorted(data, key = operator.itemgetter(0))
        self.table.data = data
        self.table.ResetView(self, currentRows)

        self.AutoSizeRows()


from wx.lib import wordwrap

class CutomGridCellAutoWrapStringRenderer(wx.grid.PyGridCellRenderer):
    def __init__(self):
        wx.grid.PyGridCellRenderer.__init__(self)

    def Draw(self, grid, attr, dc, rect, row, col, isSelected):
        text = grid.GetCellValue(row, col)
        dc.SetFont( attr.GetFont() )
        text = wordwrap.wordwrap(text, grid.GetColSize(col), dc, breakLongWords = False)
        hAlign, vAlign = attr.GetAlignment()
        if isSelected:
            bg = grid.GetSelectionBackground()
            fg = grid.GetSelectionForeground()
        else:
            bg = attr.GetBackgroundColour()
            fg = attr.GetTextColour()
        dc.SetTextBackground(bg)
        dc.SetTextForeground(fg)
        dc.SetBrush(wx.Brush(bg, wx.SOLID))
        dc.SetPen(wx.TRANSPARENT_PEN)
        dc.DrawRectangleRect(rect)
        grid.DrawTextRectangle(dc, text, rect, hAlign, vAlign)

    def GetBestSize(self, grid, attr, dc, row, col):
        text = grid.GetCellValue(row, col)
        dc.SetFont(attr.GetFont())
        text = wordwrap.wordwrap(text, grid.GetColSize(col), dc, breakLongWords = False)
        w, h, lineHeight = dc.GetMultiLineTextExtent(text)
        return wx.Size(w, h)

    def Clone(self):
        return CutomGridCellAutoWrapStringRenderer()

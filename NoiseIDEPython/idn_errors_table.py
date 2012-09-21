from exceptions import IndexError
import operator
import os
import wx
from wx.grid import PyGridTableBase
from idn_global import GetTabMgr

__author__ = 'Yaroslav'

class ErrorsTableGrid(wx.grid.Grid):
    def __init__(self, parent, project):
        wx.grid.Grid.__init__(self, parent, -1)
        self.project = project
        self.table = ErrorsTable([])
        self.SetTable(self.table, True)
        self.AutoSizeColumns(False)
        self.SetRowLabelSize(0)
        self.SetMargins(0,0)
        self.SetColSize(0, 450)
        self.SetColSize(1, 50)
        self.SetColSize(2, 100)
        self.SetColSize(3, 750)
        self.EnableEditing(False)
        self.SetColMinimalAcceptableWidth(50)

        self.DisableCellEditControl()
        self.DisableDragCell()
        self.DisableDragColMove()
        self.DisableDragColSize()
        self.DisableDragGridSize()
        self.DisableDragRowSize()
        self.Bind(wx.grid.EVT_GRID_CELL_LEFT_DCLICK, self.OnLeftDClick)

    def OnLeftDClick(self, event):
        row = event.GetRow()
        rowData = self.table.data[row]
        file = os.path.join(self.project.AppsPath(), rowData[0])
        line = rowData[1]
        GetTabMgr().LoadFileLine(file, line)

    def AddErrors(self, path, errors):
        currentRows = len(self.table.data)
        newPath = path.replace(self.project.AppsPath() + os.sep, "")
        data = list(filter(lambda x: x[0] != newPath, self.table.data))
        for e in errors:
            data.append((newPath, e.line, e.TypeToStr(), e.msg))
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
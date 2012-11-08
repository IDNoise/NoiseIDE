from exceptions import Exception
import os
import wx
import wx.lib.agw.customtreectrl as CT
import core

__author__ = 'Yaroslav'

class BasicValidator(wx.PyValidator):
    def __init__(self, title):
        wx.PyValidator.__init__(self)
        self.title = title

    def TransferToWindow(self):
        return True

    def TransferFromWindow(self):
        return True

    def Clone(self):
        return type(self)(self.title)

    def Error(self):
        wnd = self.GetWindow()
        wnd.SetBackgroundColour("pink")
        wnd.SetFocus()
        wnd.Refresh()
        return False

    def Ok(self):
        wnd = self.GetWindow()
        wnd.SetBackgroundColour(
            wx.SystemSettings_GetColour(wx.SYS_COLOUR_WINDOW))
        wnd.Refresh()
        return True


class PathExistsValidator(BasicValidator):
    def __init__(self, title, isDir = False):
        BasicValidator.__init__(self, title)
        self.isDir = isDir

    def Validate(self, win):
        textCtrl = self.GetWindow()
        text = textCtrl.GetValue()

        if self.isDir:
            result = os.path.isdir(text)
        else:
            result = os.path.isfile(text)

        if not result:
            wx.MessageBox(self.title + " must be existing " + ("dir" if self.isDir else "file"), "Error")
            return self.Error()
        else:
            return self.Ok()


class NotEmptyTextValidator(BasicValidator):
    def Validate(self, win):
        textCtrl = self.GetWindow()
        text = textCtrl.GetValue()

        if len(text) == 0:
            wx.MessageBox(self.title + " must be not empty", "Error")
            return self.Error()
        else:
            return self.Ok()


class IDNCustomTreeCtrl(CT.CustomTreeCtrl):
    def GetItemChildren(self, item):
        try:
            children = []
            child, cookie = self.GetFirstChild(item)
            while child:
                children.append(child)
                child, cookie = self.GetNextChild(item, cookie)
            return children
        except Exception, e:
            core.Log("Get item children error", e)
            return []

    def GetAllItemFiles(self, item):
        result = []
        if not self.ItemHasChildren(item):
            return result
        def gatherChildren(item):
            children = self.GetItemChildren(item)
            for c in children:
                if self.ItemHasChildren(c):
                    gatherChildren(c)
                else:
                    result.append(self.GetPyData(c))
        gatherChildren(item)
        return result
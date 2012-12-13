__author__ = 'Yaroslav Nikityshev aka IDNoise'

from wx.lib.agw import aui

class Manager(aui.AuiManager):
    def OnCaptionDoubleClicked(self, paneWindow):
        pane = self.GetPaneByWidget(paneWindow)
        if pane.name.startswith("__floating__") and pane.IsFloating():
            if pane.frame.IsMaximized():
                pane.frame.Restore()
            else:
                pane.frame.Maximize()
            self.Update()
        else:
            aui.AuiManager.OnCaptionDoubleClicked(self, paneWindow)

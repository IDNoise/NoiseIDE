__author__ = 'Yaroslav Nikityshev aka IDNoise'

from wx.lib.agw import aui

class Manager(aui.AuiManager):
#    def OnFloatingPaneActivated(self, paneWindow):
#        aui.AuiManager.OnFloatingPaneActivated(self, paneWindow)
#        pane = self.GetPaneByWidget(paneWindow)
#        print("OnFloatingPaneActivated", pane, pane.name)

#    def OnPaneDocked(self, event):
#        aui.AuiManager.OnPaneDocked(self, event)
#        pane = event.GetPane()
#        print("OnPaneDocked", pane, pane.name)

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
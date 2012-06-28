
__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
from idn_colorschema import ColorSchema
from idn_customstc import CustomSTC

class NoiseIDE(wx.Frame):
    def __init__(self, *args, **kwargs):
        wx.Frame.__init__(self, None, wx.ID_ANY, 'Noise IDE', size = (500, 900))
        ColorSchema.load("dark")
        self.TextArea = CustomSTC(self, os.path.join(os.getcwd(), "eide_cache.erl"))
        self.Show()




if __name__ == '__main__':
    def main():
        app = wx.App(redirect=False)
        frame = NoiseIDE()
        app.MainLoop()

    main()
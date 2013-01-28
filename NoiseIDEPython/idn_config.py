from idn_colorschema import ColorSchema

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
import yaml
import shutil
import core
from idn_utils import CreateButton

class Config:
    COLOR_SCHEMA = "color_schema"
    USER_NAME = "user_name"
    LAST_PROJECT_LIST = "last_project_list"
    RUNTIMES = "runtimes"
    TOOLTIP_DELAY = "tooltip_delay"

    DEFAULT_TOOLTIP_DELAY = 500

    @classmethod
    def load(cls):
        firstTime = False
        path = os.path.join(core.MainFrame.cwd, "noiseide.yaml")
        if not os.path.isfile(path):
            shutil.copy2(os.path.join(core.MainFrame.cwd, "noiseide.template.yaml"), path)
            firstTime = True
        stream = file(path, 'r')
        cls.data = yaml.load(stream)
        if firstTime:
            form = ConfigEditForm()
            form.ShowModal()

    @classmethod
    def save(cls):
        path = os.path.join(core.MainFrame.cwd, "noiseide.yaml")
        stream = file(path, 'w')
        yaml.dump(cls.data, stream)
        ColorSchema.load(cls.ColorSchema())

    @classmethod
    def GetProp(cls, prop, default = None):
        if prop in cls.data:
            return cls.data[prop]
        return default

    @classmethod
    def SetProp(cls, prop, value):
        cls.data[prop] = value
        cls.save()

    @classmethod
    def UserName(cls):
        return cls.data[cls.USER_NAME] if cls.USER_NAME in cls.data else "NoiseIDE User"

    @classmethod
    def ColorSchema(cls):
        return cls.data[cls.COLOR_SCHEMA] if cls.COLOR_SCHEMA in cls.data else "dark"

    @classmethod
    def Runtimes(cls):
        return cls.data[cls.RUNTIMES] if cls.RUNTIMES in cls.data else {}

    @classmethod
    def LastProjects(cls):
        projects = (cls.data[cls.LAST_PROJECT_LIST] if cls.LAST_PROJECT_LIST in cls.data else [])
        projects = [p for p in projects if os.path.isfile(p)]

        cls.data[cls.LAST_PROJECT_LIST] = projects
        return projects[:]

    @classmethod
    def SetLastProjects(cls, projects):
        cls.data[cls.LAST_PROJECT_LIST] = projects
        cls.save()

    @classmethod
    def TooltipDelay(cls):
        return cls.data[cls.TOOLTIP_DELAY] if cls.TOOLTIP_DELAY in cls.data else cls.DEFAULT_TOOLTIP_DELAY

    @classmethod
    def SetTooltipDelay(cls, delay):
        try:
            delay = int(delay)
        except:
            delay = cls.DEFAULT_TOOLTIP_DELAY
        finally:
            cls.data[cls.TOOLTIP_DELAY] = delay
            cls.save()

class ConfigEditForm(wx.Dialog):
    def __init__(self):
        wx.Dialog.__init__(self, core.MainFrame, size = (290, 150), title = "Edit config")

        sizer = wx.GridBagSizer(2, 2)
        self.colorSchemas = []
        for file in os.listdir(core.MainFrame.cwd):
            if file.endswith(".color.yaml"):
                self.colorSchemas.append(file.split(".")[0])
        self.colorSchemaCB = wx.ComboBox(self, choices = self.colorSchemas,
            value = Config.ColorSchema(),
            size = (180, 25),
            style = wx.CB_READONLY
        )
        self.userNameTB = wx.TextCtrl(self, value = Config.UserName(), size = (180, 25))
        self.tooltipDelayTB = wx.TextCtrl(self, value = str(Config.TooltipDelay()), size = (180, 25))
        self.closeButton = CreateButton(self, "Close", self.OnClose)
        self.saveButton = CreateButton(self, "Save", self.OnSave)

        sizer.Add(wx.StaticText(self, label = "Color schema:"), (0, 0), flag = wx.ALL | wx.ALIGN_CENTER_VERTICAL, border = 2)
        sizer.Add(self.colorSchemaCB, (0, 1), flag = wx.ALL | wx.wx.ALIGN_LEFT, border = 2)
        sizer.Add(wx.StaticText(self, label = "User name:"), (1, 0), flag = wx.ALL | wx.ALIGN_CENTER_VERTICAL, border = 2)
        sizer.Add(self.userNameTB, (1, 1), flag = wx.ALL | wx.wx.ALIGN_LEFT, border = 2)
        sizer.Add(wx.StaticText(self, label = "Tooltip delay:"), (2, 0), flag = wx.ALL | wx.ALIGN_CENTER_VERTICAL, border = 2)
        sizer.Add(self.tooltipDelayTB, (2, 1), flag = wx.ALL | wx.wx.ALIGN_LEFT, border = 2)


        sizer.Add(self.closeButton, (3, 0), flag = wx.ALL | wx.wx.ALIGN_LEFT, border = 2)
        sizer.Add(self.saveButton, (3, 1), flag = wx.ALL | wx.wx.ALIGN_RIGHT, border = 2)

        self.SetSizer(sizer)
        self.Layout()

    def OnClose(self, event):
        self.Close()

    def OnSave(self, event):
        if self.colorSchemaCB.Value in self.colorSchemas:
            Config.SetProp(Config.COLOR_SCHEMA, self.colorSchemaCB.Value)
        if self.userNameTB.Value:
            Config.SetProp(Config.USER_NAME, self.userNameTB.Value)
        if self.tooltipDelayTB.Value:
            Config.SetTooltipDelay(self.tooltipDelayTB.Value)
        self.Close()

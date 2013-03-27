from idn_colorschema import ColorSchema

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import os
import wx
import yaml
import shutil
import core
from idn_utils import CreateButton, readFile


class Config:
    COLOR_SCHEMA = "color_schema"
    USER_NAME = "user_name"
    OPEN_LAST_FILES = "open_last_files"
    LAST_PROJECT_LIST = "last_project_list"
    RUNTIMES = "runtimes"
    TOOLTIP_DELAY = "tooltip_delay"
    REFRESH_INTERVAL = "refresh_interval"
    SHOW_MULTI_COMPLETE = "show_multi_complete"

    DEFAULT_TOOLTIP_DELAY = 500
    DEFAULT_REFRESH_INTERVAL = 2

    data = {}

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
            cls.SetProp("current_version", cls.GetCurrentVersion())
            cls.save()
            form = ConfigEditForm()
            form.ShowModal()

    @classmethod
    def GetCurrentVersion(cls):
        revCfg = os.path.join(core.MainFrame.cwd, "rev.cfg")
        version = 0.1
        if os.path.isfile(revCfg):
            data = readFile(revCfg)
            version = float(data.split("\n")[0].split(":")[1].strip())
        return version

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
    def ShowMultiComplete(cls):
        return cls.data[cls.SHOW_MULTI_COMPLETE] if cls.SHOW_MULTI_COMPLETE in cls.data else False

    @classmethod
    def AvailableRuntimes(cls):
        result = {}
        if Config.Runtimes():
            for r in Config.Runtimes():
                if  os.path.isfile(Config.Runtimes()[r]):
                    result[r] = Config.Runtimes()[r]
        return result

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
    def RefreshInterval(cls):
        return cls.data[cls.REFRESH_INTERVAL] if cls.REFRESH_INTERVAL in cls.data else cls.DEFAULT_REFRESH_INTERVAL

    @classmethod
    def SetTooltipDelay(cls, delay):
        try:
            delay = int(delay)
        except:
            delay = cls.DEFAULT_TOOLTIP_DELAY
        finally:
            cls.data[cls.TOOLTIP_DELAY] = delay
            cls.save()

    @classmethod
    def SetRefreshInterval(cls, interval):
        try:
            interval = abs(int(interval))
        except:
            interval = cls.DEFAULT_REFRESH_INTERVAL
        finally:
            cls.data[cls.REFRESH_INTERVAL] = interval
            cls.save()
        if core.Project:
            core.Project.SetRefreshInterval(interval)

class ConfigEditForm(wx.Dialog):
    def __init__(self):
        wx.Dialog.__init__(self, core.MainFrame, title = "Edit config")
        wx.ToolTip_Enable(True)
        sizer = wx.GridBagSizer(2, 2)
        self.colorSchemas = []
        for f in os.listdir(core.MainFrame.cwd):
            if f.endswith(".color.yaml"):
                self.colorSchemas.append(f.split(".")[0])
        self.colorSchemaCB = wx.ComboBox(self, choices = self.colorSchemas,
            value = Config.ColorSchema(),
            size = (180, 25),
            style = wx.CB_READONLY
        )
        self.userNameTB = wx.TextCtrl(self, value = Config.UserName(), size = (180, 25))
        self.tooltipDelayTB = wx.TextCtrl(self, value = str(Config.TooltipDelay()), size = (180, 25))
        self.refreshIntervalTB = wx.TextCtrl(self, value = str(Config.RefreshInterval()), size = (180, 25))
        self.openLastFilesCb = wx.CheckBox(self, label = "Open last files")
        self.openLastFilesCb.SetValue(Config.GetProp(Config.OPEN_LAST_FILES, True))
        self.showMultiCompleteCb = wx.CheckBox(self, label = "Show multi complete")
        self.showMultiCompleteCb.SetValue(Config.ShowMultiComplete())
        self.refreshIntervalTB.SetToolTipString("Interval in seconds. 0 = No refresh")
        self.closeButton = CreateButton(self, "Close", self.OnClose)
        self.saveButton = CreateButton(self, "Save", self.OnSave)

        i = 0
        sizer.Add(wx.StaticText(self, label = "Color schema:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER_VERTICAL, border = 2)
        sizer.Add(self.colorSchemaCB, (i, 1), flag = wx.ALL | wx.wx.ALIGN_LEFT, border = 2)
        i += 1
        sizer.Add(wx.StaticText(self, label = "User name:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER_VERTICAL, border = 2)
        sizer.Add(self.userNameTB, (i, 1), flag = wx.ALL | wx.wx.ALIGN_LEFT, border = 2)
        i += 1
        sizer.Add(wx.StaticText(self, label = "Tooltip delay:"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER_VERTICAL, border = 2)
        sizer.Add(self.tooltipDelayTB, (i, 1), flag = wx.ALL | wx.wx.ALIGN_LEFT, border = 2)
        i += 1
        sizer.Add(self.openLastFilesCb, (i, 0), flag = wx.ALL | wx.ALIGN_LEFT, border = 2)
        i += 1
        sizer.Add(wx.StaticText(self, label = "Tree refresh interval(seconds):"), (i, 0), flag = wx.ALL | wx.ALIGN_CENTER_VERTICAL, border = 2)
        sizer.Add(self.refreshIntervalTB, (i, 1), flag = wx.ALL | wx.ALIGN_LEFT, border = 2)
        i += 1
        sizer.Add(self.showMultiCompleteCb, (i, 1), flag = wx.ALL | wx.ALIGN_LEFT, border = 2)
        i += 1
        sizer.Add(self.closeButton, (i, 0), flag = wx.ALL | wx.wx.ALIGN_LEFT, border = 2)
        sizer.Add(self.saveButton, (i, 1), flag = wx.ALL | wx.wx.ALIGN_RIGHT, border = 2)

        self.SetSizer(sizer)
        self.Layout()
        sizer.SetSizeHints(self)

    def OnClose(self, event):
        self.Close()

    def OnSave(self, event):
        if self.colorSchemaCB.Value in self.colorSchemas:
            Config.SetProp(Config.COLOR_SCHEMA, self.colorSchemaCB.Value)
        if self.userNameTB.Value:
            Config.SetProp(Config.USER_NAME, self.userNameTB.Value)
        if self.tooltipDelayTB.Value:
            Config.SetTooltipDelay(self.tooltipDelayTB.Value)
        if self.refreshIntervalTB.Value:
            Config.SetRefreshInterval(self.refreshIntervalTB.Value)
        Config.SetProp(Config.OPEN_LAST_FILES, self.openLastFilesCb.Value)
        Config.SetProp(Config.SHOW_MULTI_COMPLETE, self.showMultiCompleteCb.Value)
        self.Close()

import os
from idn_completer import Completer
import core
import yaml

class MacrosCompleter(Completer):
    def __init__(self, stc):
        Completer.__init__(self, stc)

        self.macroses = []

        for path in [os.path.join(core.MainFrame.cwd, "data", "erlang", "ide_macros.yaml"),
                     os.path.join(core.MainFrame.cwd, "data", "erlang", "user_macros.yaml")]:
            if (os.path.exists(path)):
                stream = file(path, 'r')
                data = yaml.load(stream)
                if data:
                    self.macroses += data



    def OnUpdate(self, text, nextChar = None):
        self.list.Clear()

        core.Log(text)
        i = len(text) - 1
        while i >= 0 and text[i].isalpha():
            self.prefix += text[i]
            i -= 1
        self.prefix = self.prefix[::-1]
        core.Log(self.prefix)
        for macros in self.macroses:
            if self.prefix == "" or macros['id'].startswith(self.prefix):
                self.list.Append(macros['id'], macros['desc'] + "</br></br>" + macros['macros'])

    def AutoComplete(self, text):
        macros = ""
        for m in self.macroses:
            if m['id'] == text:
                macros = m['macros']
        if not macros: return

        startPos = self.stc.GetCurrentPos() - len(self.prefix)
        self.stc.SetSelectionStart(startPos)
        self.stc.SetSelectionEnd(self.stc.GetCurrentPos())

        self.stc.ReplaceSelection(macros)
        self.HideCompleter()
        self.stc.StartMacroEditing(startPos, macros)

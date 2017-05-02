import os
from idn_completer import Completer
import core
import yaml

class SnippetCompleter(Completer):
    def __init__(self, stc):
        Completer.__init__(self, stc)

        self.snippets = []

        for path in [os.path.join(core.MainFrame.cwd, "data", "erlang", "ide_snippets.yaml"),
                     os.path.join(core.MainFrame.cwd, "data", "erlang", "user_snippets.yaml"),
                     os.path.join(core.Project.projectDir, "snippets.yaml")]:
            if os.path.exists(path):
                stream = file(path, 'r')
                data = yaml.load(stream)
                if data:
                    self.snippets += data



    def OnUpdate(self, text, nextChar = None):
        self.list.Clear()

        core.Log(text)
        i = len(text) - 1
        while i >= 0 and text[i].isalpha():
            self.prefix += text[i]
            i -= 1
        self.prefix = self.prefix[::-1]
        core.Log(self.prefix)
        for snippet in self.snippets:
            if self.prefix == "" or snippet['id'].startswith(self.prefix):
                self.list.Append(snippet['id'], snippet['desc'] + "<br/><br/>" + snippet['snippet'])

    def AutoComplete(self, text):
        snippet = ""
        for m in self.snippets:
            if m['id'] == text:
                snippet = m['snippet']
        if not snippet: return

        startPos = self.stc.GetCurrentPos() - len(self.prefix)
        self.stc.SetSelectionStart(startPos)
        self.stc.SetSelectionEnd(self.stc.GetCurrentPos())

        self.stc.ReplaceSelection(snippet)
        self.HideCompleter()
        self.stc.StartSnippetEditing(startPos, snippet)

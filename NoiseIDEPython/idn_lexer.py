__author__ = 'Yaroslav Nikityshev aka IDNoise'


class BaseLexer:
    def __init__(self, stc):
        self.stc = stc

    def StyleText(self, startPos, endPos):
        raise NotImplementedError

    def DoFold(self, startPos, endPos):
        raise NotImplementedError

    def StyleEvent(self, event):
        self.stc = event.GetEventObject()
        startPos = self.stc.GetEndStyled()
        endPos = event.GetPosition()
        self.StyleText(startPos, endPos)
        self.DoFold(startPos, endPos)


from wx.stc import STC_FOLDLEVELHEADERFLAG, STC_FOLDLEVELBASE

__author__ = 'Yaroslav Nikityshev aka IDNoise'

from idn_highlight import ErlangHighlighter, ErlangHighlightType

class BaseLexer:
    def StyleText(self, control, startPos, endPos):
        raise NotImplementedError

    def DoFold(self, control, startPos, endPos):
        raise NotImplementedError

    def StyleEvent(self, event):
        control = event.GetEventObject()
        startPos = control.GetEndStyled()
        endPos = event.GetPosition()
        self.StyleText(control, startPos, endPos)
        self.DoFold(control, startPos, endPos)


class ErlangLexer(BaseLexer):
    def __init__(self):
        self.highlighter = ErlangHighlighter()

    def StyleText(self, control, startPos, endPos):
        startLine = control.LineFromPosition(startPos)
        startLineBeginPos = control.PositionFromLine(startLine)
        endLine = control.LineFromPosition(endPos)
        endLineEndPos = control.GetLineEndPosition(endLine)
        control.StartStyling(startLineBeginPos, 0x1f)
        lastEnd = startLineBeginPos
        defaultStyle = ErlangHighlightType.DEFAULT
        while startLine <= endLine:
            lineStart = control.PositionFromLine(startLine)
            text = control.GetLine(startLine)
            tokens = self.highlighter.GetHighlightingTokens(text)
            for token in tokens:
                start = lineStart + token.start
                if start > lastEnd:
                    control.SetStyling(start - lastEnd, defaultStyle)
                control.SetStyling(len(token.value), token.type)
                lastEnd  = lineStart + token.end
            startLine += 1

        if lastEnd < endLineEndPos:
            control.SetStyling(endLineEndPos - lastEnd, defaultStyle)

    def DoFold(self, control, startPos, endPos):
        startLine = control.LineFromPosition(startPos) - 1
        endLine = control.LineFromPosition(endPos)
        prevFoldLevel = 0
        if startLine > 0:
            prevFoldLevel = control.GetFoldLevel(startLine - 1)
        nextLineFoldLevel = prevFoldLevel
        if prevFoldLevel ^ STC_FOLDLEVELHEADERFLAG == STC_FOLDLEVELBASE:
            nextLineFoldLevel = STC_FOLDLEVELBASE + 1
        elif prevFoldLevel == STC_FOLDLEVELBASE + 2:
            nextLineFoldLevel = 0
        while startLine <= endLine:
            currentLineFoldLevel = nextLineFoldLevel
            text = control.GetLine(startLine)
            tokens = self.highlighter.GetHighlightingTokens(text)
            for token in tokens:
                if (token.type in {ErlangHighlightType.FUNDEC,
                                   ErlangHighlightType.RECORDDEF,
                                   ErlangHighlightType.SPEC}):
                    currentLineFoldLevel = STC_FOLDLEVELBASE
                    nextLineFoldLevel = STC_FOLDLEVELBASE  + 1
                elif token.type == ErlangHighlightType.FULLSTOP:
                    if currentLineFoldLevel ==  STC_FOLDLEVELBASE  + 1:
                        currentLineFoldLevel = STC_FOLDLEVELBASE  + 2
                    elif currentLineFoldLevel == STC_FOLDLEVELBASE:
                        currentLineFoldLevel = 0
                        if prevFoldLevel == STC_FOLDLEVELHEADERFLAG | STC_FOLDLEVELBASE:
                            control.SetFoldLevel(startLine - 1, 0)
                    nextLineFoldLevel = 0
            if currentLineFoldLevel == STC_FOLDLEVELBASE:
                currentLineFoldLevel |= STC_FOLDLEVELHEADERFLAG
            if (currentLineFoldLevel == STC_FOLDLEVELHEADERFLAG | STC_FOLDLEVELBASE and
                currentLineFoldLevel == prevFoldLevel):
                control.SetFoldLevel(startLine - 1, 0)
            prevFoldLevel = currentLineFoldLevel
            control.SetFoldLevel(startLine, currentLineFoldLevel)
            startLine += 1
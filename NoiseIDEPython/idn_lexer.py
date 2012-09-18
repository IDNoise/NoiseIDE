import re

__author__ = 'Yaroslav Nikityshev aka IDNoise'

from wx.stc import STC_FOLDLEVELHEADERFLAG, STC_FOLDLEVELBASE
from idn_highlight import ErlangHighlighter, ErlangHighlightType


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

class RecordStart:
    def __init__(self, record, start):
        self.record = record
        self.start = start

class LineData:
    def __init__(self):
        self.functionName = None
        self.functionStart = None
        self.functionEnd = None
#        self.starts = []
#        self.lastClose
#
#    def AddRecordStart(self, record, start):
#        self.starts.append(RecordStart(record, start))
#
#    def AddTupleStart(self, start):
#        self.starts.append(start)
#
#    def AddEnd(self, end):


class ErlangLexer(BaseLexer):
    def __init__(self, stc):
        BaseLexer.__init__(self, stc)
        self.highlighter = ErlangHighlighter()
        self.linesData = {}

    def StyleText(self, startPos, endPos):
        startLine = self.stc.LineFromPosition(startPos)
        startLineBeginPos = self.stc.PositionFromLine(startLine)
        endLine = self.stc.LineFromPosition(endPos)
        endLineEndPos = self.stc.GetLineEndPosition(endLine)
        self.stc.StartStyling(startLineBeginPos, 0x1f)
        lastEnd = startLineBeginPos
        defaultStyle = ErlangHighlightType.DEFAULT
        while startLine <= endLine:
            lineData = LineData()
            self.linesData[startLine] = lineData
            lineStart = self.stc.PositionFromLine(startLine)
            text = self.stc.GetLine(startLine)
            tokens = self.highlighter.GetHighlightingTokens(text)
            for token in tokens:
                start = lineStart + token.start
                if start > lastEnd:
                    self.stc.SetStyling(start - lastEnd, defaultStyle)
                self.stc.SetStyling(len(token.value), token.type)
                lastEnd  = lineStart + token.end
#            self.UpdateLineData(startLine, tokens)
                if token.type == ErlangHighlightType.FUNDEC:
                    lineData.functionName = token.value
                    lineData.functionStart = token.start + lineStart
                if token.type == ErlangHighlightType.FULLSTOP:
                    lineData.functionEnd = token.end + lineStart
                    line = startLine
                    while line > 0:
                        if self.linesData[line].functionName != None:
                            self.linesData[line].functionEnd = token.end + lineStart
                            break
                        line -= 1

            startLine += 1

        if lastEnd < endLineEndPos:
            self.stc.SetStyling(endLineEndPos - lastEnd, defaultStyle)

    def DoFold(self, startPos, endPos):
        startLine = self.stc.LineFromPosition(startPos) - 1
        endLine = self.stc.LineFromPosition(endPos)
        prevFoldLevel = 0
        if startLine > 0:
            prevFoldLevel = self.stc.GetFoldLevel(startLine - 1)
        nextLineFoldLevel = prevFoldLevel
        if prevFoldLevel ^ STC_FOLDLEVELHEADERFLAG == STC_FOLDLEVELBASE:
            nextLineFoldLevel = STC_FOLDLEVELBASE + 1
        elif prevFoldLevel == STC_FOLDLEVELBASE + 2:
            nextLineFoldLevel = 0
        while startLine <= endLine:
            currentLineFoldLevel = nextLineFoldLevel
            text = self.stc.GetLine(startLine)
            tokens = self.highlighter.GetHighlightingTokens(text)
            for token in tokens:
                if (token.type in {ErlangHighlightType.FUNDEC, ErlangHighlightType.RECORDDEF}
                    or token.value == "-spec"):
                    currentLineFoldLevel = STC_FOLDLEVELBASE
                    nextLineFoldLevel = STC_FOLDLEVELBASE  + 1
                elif token.type == ErlangHighlightType.FULLSTOP:
                    if currentLineFoldLevel ==  STC_FOLDLEVELBASE  + 1:
                        currentLineFoldLevel = STC_FOLDLEVELBASE  + 2
                    elif currentLineFoldLevel == STC_FOLDLEVELBASE:
                        currentLineFoldLevel = 0
                        if prevFoldLevel == STC_FOLDLEVELHEADERFLAG | STC_FOLDLEVELBASE:
                            self.stc.SetFoldLevel(startLine - 1, 0)
                    nextLineFoldLevel = 0
            if currentLineFoldLevel == STC_FOLDLEVELBASE:
                currentLineFoldLevel |= STC_FOLDLEVELHEADERFLAG
            if (currentLineFoldLevel == STC_FOLDLEVELHEADERFLAG | STC_FOLDLEVELBASE and
                currentLineFoldLevel == prevFoldLevel):
                self.stc.SetFoldLevel(startLine - 1, 0)
            prevFoldLevel = currentLineFoldLevel
            self.stc.SetFoldLevel(startLine, currentLineFoldLevel)
            startLine += 1

    def IsInFunction(self):
        line = self.stc.GetCurrentLine()
        caretPos = self.stc.GetCurrentPos()
        while line > 0:
            data = self.linesData[line]
            if data.functionEnd and caretPos > data.functionEnd: return False
            if data.functionName: return data.functionEnd >= caretPos
            line -= 1
        return False

    def GetCurrentFunction(self):
        line = self.stc.GetCurrentLine()
        caretPos = self.stc.GetCurrentPos()
        while line > 0:
            data = self.linesData[line]
            if data.functionEnd and caretPos > data.functionEnd:
                break
            if self.linesData[line].functionName:
                if data.functionEnd >= caretPos:
                    return (data.functionName,
                            data.functionStart,
                            data.functionEnd,
                            self.stc.GetTextRange(data.functionStart, data.functionEnd))
                else:
                    break
            line -= 1
        return None


    def RecordFieldUnderCursor(self):
        opened = '({['
        closed = ')}]'
        comma = ','
        eq = ','
        recordOpenBracket = "{"
        constructs = {"case", "try", "receive", "begin", "if"}
        stateEnd = 1
        stateClosed = 2
        caretPos = self.stc.GetCurrentPos()
        #print caretPos
        if self.IsInFunction():
            #print "in func"
            funData = self.GetCurrentFunction()
            text = funData[3]
            text = text[:caretPos - funData[1]]
        else:
            #print "not in func"
            line = self.stc.GetCurrentLine()
            text = self.stc.GetTextRange(self.stc.PositionFromLine(line - 10), caretPos)
        #print text
        tokens = self.highlighter.GetHighlightingTokens(text)
        tokens.reverse()
        tokens  = [token for token in tokens if token.type not in [ErlangHighlightType.COMMENT]]
        result = False
        open = 0
        record = ""
        #print(tokens)
        prefix = ""
        if len(tokens) > 1 and tokens[0].type== ErlangHighlightType.ATOM and tokens[1].value in [comma, recordOpenBracket]:
            prefix = tokens[0].value
        elif len(tokens) > 0 and tokens[0].value in [comma, recordOpenBracket]:
            prefix = ""
        else:
            return (False, "", "")
        state = None
        first = None
        lastBracket = None
        #print("start -----------------------")
        for i, token in enumerate(tokens):
            #print("state:{}, token:{}, lastBracket:{}".format(state, (token.type, token.value), lastBracket))
            if not state:
                if token.value == comma:
                    first = comma
                elif token.value == eq:
                    if first != comma:
                        break
                elif token.type == ErlangHighlightType.BRACKET and token.value == recordOpenBracket:
                    if len(tokens) > i+1 and tokens[i+1].type == ErlangHighlightType.RECORD:
                        result = True
                        record = tokens[i+1].value[1:]
                    break
                elif token.value == "end":
                    state = stateEnd
                elif token.value in closed:
                    state = stateClosed
                    lastBracket = (token.value, None)
                elif token.value in "([" or token.value in constructs:
                    break
            elif state == stateEnd and token.value in constructs:
                state = None
            elif state == stateClosed and token.value in closed:
                if lastBracket:
                    lastBracket = (token.value, lastBracket)
                else:
                    lastBracket = (token.value, None)
            elif state == stateClosed and token.value == opened[closed.index(lastBracket[0])]:
                if lastBracket[1]:
                    lastBracket = lastBracket[1]
                else:
                    state = None
            #print("end -----------------------")
        return (result, record, prefix)

    def GetAllExports(self):
        regexp = "^-export\(\[(.*?)\s*\]\)\."
        r = re.compile(regexp, re.MULTILINE | re.DOTALL)
        text = self.stc.GetText()
        pos = 0
        result = ""
        lastInsertPosition = None
        while True:
            match = r.search(text, pos)
            if not match:
                break
            pos = match.end(0)
            lastInsertPosition = match.end(1)
            result += match.group(1)
        return (result, lastInsertPosition)

#    def UpdateLineData(self, line, tokens):
#        data = LineData()
#        lineStart = self.stc.PositionFromLine(line)
#        for i, token in enumerate(tokens):
#            prevToken = tokens[i - 1] if i > 0 else None
#            if token.type == ErlangHighlightType.BRACKET:
#                if token.value == "{":
#                    if prevToken and prevToken.type == ErlangHighlightType.RECORD:
#                        data.AddRecordStart(prevToken.value, lineStart + token.start)
#                    else:
#                        data.AddTupleStart(lineStart + token.start)
#                if token.value == "}":
#                    if data.starts
#
#        self.lineData[line] = data
                
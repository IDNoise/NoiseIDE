import re
from wx.stc import STC_FOLDLEVELHEADERFLAG, STC_FOLDLEVELBASE
from idn_highlight import ErlangHighlighter, ErlangHighlightType, IgorHighlighter, IgorHighlightType
from idn_lexer import BaseLexer
from idn_erlang_utils import IsModule

__author__ = 'Yaroslav'

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
            prevData = self.linesData[startLine] if startLine in self.linesData else None

            if prevData and prevData.functionEnd:
                line = startLine
                while line > 0:
                    if self.linesData[line].functionName != None and self.linesData[line].functionEnd == prevData.functionEnd:
                        self.linesData[line].functionEnd = None
                        break
                    line -= 1

            lineData = LineData()
            self.linesData[startLine] = lineData
            lineStart = self.stc.PositionFromLine(startLine)
            text = self.stc.GetLineUTF8(startLine)
            tokens = self.highlighter.GetHighlightingTokens(text)
            for token in tokens:
                start = lineStart + token.start
                if start > lastEnd:
                    self.stc.SetStyling(start - lastEnd, defaultStyle)
                self.stc.SetStyling(len(token.value), token.type)
                lastEnd  = lineStart + token.end
                if token.type == ErlangHighlightType.FUNDEC:
                    lineData.functionName = token.value
                    lineData.functionStart = token.start + lineStart
                    line = startLine - 1
                    while line > 0:
                        if self.linesData[line].functionName == token.value:
                            self.linesData[line].functionEnd = self.stc.GetLineEndPosition(startLine - 1)
                            break
                        line -= 1
                elif token.type == ErlangHighlightType.FUNCTION:
                    if tokens[0].value == "-spec":
                        lineData.specName = tokens[1].value
                        lineData.specStart = tokens[1].start + lineStart
                elif token.type == ErlangHighlightType.FULLSTOP:
                    lineData.functionEnd = token.end + lineStart
                    line = startLine
                    while line > 0:
                        if self.linesData[line].functionName != None and self.linesData[line].functionEnd == None:
                            self.linesData[line].functionEnd = token.end + lineStart
                            break
                        elif self.linesData[line].specName != None and self.linesData[line].specEnd == None:
                            self.linesData[line].specEnd = token.end + lineStart
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
            text = self.stc.GetLineUTF8(startLine)
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
            if data.functionName: return not data.functionEnd or data.functionEnd >= caretPos
            line -= 1
        return False

    def GetCurrentFunction(self, line = None, caretPos = None):
        if line is None:
            line = self.stc.GetCurrentLine()
        if caretPos is None:
            caretPos = self.stc.GetCurrentPos()
        while line > 0:
            data = self.linesData[line]
            if data.functionEnd and caretPos > data.functionEnd:
                break
            if data.functionName:
                end =  data.functionEnd
                if not end:
                    end = caretPos
                if end >= caretPos:
                    return (data.functionName,
                            data.functionStart,
                            end,
                            self.stc.GetTextRangeUTF8(data.functionStart, end))
                else:
                    break
            line -= 1
        return None

    def GetFunctionSpec(self, line, functionName):
        startLine = line;
        while line > 0:
            data = self.linesData[line]
            if data.functionName:
                return None
            if data.specName == functionName:
                return self.stc.PositionFromLine(line), self.stc.GetTextRangeUTF8(self.stc.PositionFromLine(line), self.stc.GetLineEndPosition(startLine))
            line -= 1

    def IsInSpec(self):
        line = self.stc.GetCurrentLine()
        caretPos = self.stc.GetCurrentPos()
        while line > 0:
            data = self.linesData[line]
            if data.functionEnd or data.functionStart or data.functionName:
                return False
            if data.specEnd: return data.specEnd > caretPos
            line -= 1
        return False


    def RecordFieldUnderCursor(self):
        opened = '({['
        closed = ')}]'
        comma = ','
        eq = '='
        recordOpenBracket = "{"
        constructs = {"case", "try", "receive", "begin", "if"}
        stateEnd = 1
        stateClosed = 2
        caretPos = self.stc.GetCurrentPos()
        if self.IsInFunction():
            funData = self.GetCurrentFunction()
            text = funData[3]
            text = text[:caretPos - funData[1]]
        else:
            line = self.stc.GetCurrentLine()
            text = self.stc.GetTextRangeUTF8(self.stc.PositionFromLine(line - 10), caretPos)
        tokens = self.highlighter.GetHighlightingTokens(text)
        tokens.reverse()
        tokens  = [token for token in tokens if token.type not in [ErlangHighlightType.COMMENT]]
        result = False
        record = ""
        prefix = ""
        if len(tokens) > 1 and tokens[0].type == ErlangHighlightType.ATOM and tokens[1].value in [comma, recordOpenBracket]:
            prefix = tokens[0].value
        elif len(tokens) > 0 and tokens[0].value in [comma, recordOpenBracket]:
            prefix = ""
        else:
            return (False, "", "")
        state = None
        first = None
        lastBracket = None
        for i, token in enumerate(tokens):
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
        return (result, record, prefix)

    def IsInTypeBlock(self):
        r = re.compile(r"(?:^-spec|^-callback|^-type|^-record)(.*?)(?:^[a-z].*?|^-[a-z]+|\.)", re.MULTILINE | re.DOTALL)
        text = self.stc.GetTextUTF8()
        caretPos = self.stc.GetCurrentPos()
        pos = 0
        while True:
            match = r.search(text, pos)
            if not match: return False
            if caretPos > match.start() and caretPos < match.end():
                return True
            pos = match.end()
        return False

    def GetAllExports(self):
        r = re.compile("^-export\(\[\s*(.*?)\s*\]\)\.", re.MULTILINE | re.DOTALL)
        text = self.stc.GetTextUTF8()
        pos = 0
        result = None
        lastInsertPosition = None
        start = None
        ranges = []
        while True:
            match = r.search(text, pos)
            if not match:
                if result is None:
                    mre = re.compile("^-module\(.*?\)\.", re.MULTILINE | re.DOTALL)
                    match = mre.search(text, 0)
                    if match:
                        end = match.end()
                    else:
                        end = 0
                    if IsModule(self.stc.filePath):
                        self.stc.InsertTextUTF8(end, "\n-export([\n]).")
                        return self.GetAllExports()
                    else:
                        break
                else:
                    break
            if not start:
                start = match.start(1)
            pos = match.end(0)
            ranges.append((match.start(1), match.end(1)))
            lastInsertPosition = match.end(1)
            if result is None:
                result = ""
            result += match.group(1)
        if result is None:
            result = ""
        return result.strip(), start, pos, lastInsertPosition, ranges

    def GetExportInsertPosition(self):
        r = re.compile("^-include.*?\)\.", re.MULTILINE | re.DOTALL)
        text = self.stc.GetTextUTF8()
        pos = 0
        start = None
        while True:
            match = r.search(text, pos)
            if not match:
                if start is None:
                    mre = re.compile("^-module\(.*?\)\.", re.MULTILINE | re.DOTALL)
                    match = mre.search(text, 0)
                    if match:
                        end = match.end()
                    else:
                        end = 0
                    self.stc.InsertTextUTF8(end, "\n")
                    return end + 1
                else:
                    break
            if not start:
                start = match.start(0)
            pos = match.end(0)
        self.stc.InsertTextUTF8(pos, "\n")
        return pos + 1

class LineData:
    def __init__(self):
        self.functionName = None
        self.functionStart = None
        self.functionEnd = None
        self.specName = None
        self.specStart = None
        self.specEnd = None

    def __str__(self):
        return "Fun: {} ({}, {}). Spec: {} ({}, {})".format(self.functionName, self.functionStart, self.functionEnd,
                                                           self.specName, self.specStart, self.specEnd)

class RecordStart:
    def __init__(self, record, start):
        self.record = record
        self.start = start

class IgorLexer(BaseLexer):
    def __init__(self, stc):
        BaseLexer.__init__(self, stc)
        self.highlighter = IgorHighlighter()

    def StyleText(self, startPos, endPos):
        startLine = self.stc.LineFromPosition(startPos)
        startLineBeginPos = self.stc.PositionFromLine(startLine)
        endLine = self.stc.LineFromPosition(endPos)
        endLineEndPos = self.stc.GetLineEndPosition(endLine)
        self.stc.StartStyling(startLineBeginPos, 0x1f)
        lastEnd = startLineBeginPos
        defaultStyle = IgorHighlightType.DEFAULT
        while startLine <= endLine:
            lineStart = self.stc.PositionFromLine(startLine)
            text = self.stc.GetLineUTF8(startLine)
            tokens = self.highlighter.GetHighlightingTokens(text)
            for token in tokens:
                start = lineStart + token.start
                if start > lastEnd:
                    self.stc.SetStyling(start - lastEnd, defaultStyle)
                self.stc.SetStyling(len(token.value), token.type)
                lastEnd  = lineStart + token.end
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
            text = self.stc.GetLineUTF8(startLine)
            tokens = self.highlighter.GetHighlightingTokens(text)
            for token in tokens:
                if (token.value in ["record", "enum", "service", "variant"]):
                    currentLineFoldLevel = STC_FOLDLEVELBASE
                    nextLineFoldLevel = STC_FOLDLEVELBASE  + 1
                elif token.value == "}":
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

import re

__author__ = 'Yaroslav Nikityshev aka IDNoise'

class Token:
    def __init__(self, type, value, start, end):
        self.type = type
        self.value = value
        self.start = start
        self.end = end

    def __str__(self):
        return "Token [{} - {}] value: {}, type: {}.".format(self.start, self.end, self.value, self.type)

    def __repr__(self):
        return self.__str__()


class ErlangTokenType:
    COMMENT = "comment"
    ARROW = "arrow"
    STRING = "string"
    FUNDEC = "fundec"
    VAR = "var"
    PREPROC = "preproc"
    RECORD = "record"
    NUMBER = "number"
    MACROS = "macros"
    ATOM = "atom"
    OB = "ob"
    CB = "cb"
    OPERATOR = "operator"
    FULLSTOP = "fullstop"
    OTHER = "other"
    SPACE = "space"

class ErlangTokenizer:
    def __init__(self):
        self.tokenRegexp = re.compile(
            r"""
            (?P<comment>%.*$)
            |(?P<arrow>->)
            |(?P<string>"([^"\\]|\\.)*")
            |(?P<fundec>^(?!fun\()[a-z][a-zA-Z_0-9]*\()
            |(?P<var>[A-Z_][a-zA-Z_0-9]*)
            |(?P<preproc>^-[a-z][a-z_]*)
            |(?P<record>\#[a-z][a-z_]*)
            |(?P<number>[0-9]*\.?[0-9]+)
            |(?P<macros>\?[a-zA-Z][a-zA-Z_0-9]*)
            |(?P<atom>'?[a-zA-Z_0-9]+'?|[a-z][a-zA-Z_0-9]*)
            |(?P<ob>\(|\[|{)
            |(?P<cb>\)|\]|})
            |(?P<operator>\+|\-|/|\*|<=|=<|>=|==|=/=|=:=|<-|!)
            |(?P<fullstop>\s*\.\s*$)
            |(?P<other>\S)
            |(?P<space>\s+)
            """,
            re.VERBOSE | re.MULTILINE)

    def GetTokens(self, text):
        tokens = []
        pos = 0
        while True:
            m = self.tokenRegexp.search(text, pos)
            if not m: break
            pos = m.end()
            type = m.lastgroup
            value = m.group(type)
            tokens.append(Token(type, value, m.start(), m.end()))
        return tokens


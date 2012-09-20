__author__ = 'Yaroslav Nikityshev aka IDNoise'

import re

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
    MODULEATTR = "moduleattr"
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
            |(?P<moduleattr>^-[a-z][a-z_]*)
            |(?P<number>[0-9]{1,2}\#[0-9a-z]*|[0-9]*\.?[0-9]+)
            |(?P<record>\#[a-z][a-z_]*)
            |(?P<macros>\?[a-zA-Z][a-zA-Z_0-9]*)
            |(?P<atom>'.+?'|[a-z][a-zA-Z_0-9]*)
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
        lastValue = None
        pos = 0
        while True:
            m = self.tokenRegexp.search(text, pos)
            if not m: break
            type = m.lastgroup
            value = m.group(type)
            pos = m.end()
            tokens.append(Token(type, value, m.start(), m.end()))
        return tokens


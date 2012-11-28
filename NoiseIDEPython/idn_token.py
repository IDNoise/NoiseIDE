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
            |(?P<string>"([^"\\]|\\.)*"|"[^"\\]*?$)
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

class IgorTokenType:
    STRING = "string"
    NUMBER = "number"
    VALUE = "value"
    SPACE = "space"
    BRACKET = "bracket"
    OTHER = "other"

#Module (?P<module>\s*module\s*(?P<mname>.*?)\s*$)
#Parse attributes: (?P<attribute>\[\s*(?P<akw1>[a-z]+)?:?(?P<akw2>[a-z]+)?\s*(?P<aawpairs>.*?)\s*\]$)
#->parse attr key attr value: (?P<akey>[a-z]+)(=((?P<avalued>[\d\.]+)|(?P<avaluew>[a-zA-Z_]+)|(?P<avalues>".*?")))?
#Comment:(?P<comment>//.*?$|/\*.*?\*/) //dotall
#Record: (?P<record>(record|variant)\s*(?P<rname>[A-Za-z0-9_\.]*)\s*\{(?P<rcontent>.*?)\}) //dotall
#->record data: (\[(?P<atag>tag)\]|\s*(?P<ftype>.*?)\s*(?P<fname>[a-z]+)\s*(=\s*((?P<fvalued>[\d\.]+)|(?P<fvaluew>[a-zA-Z_]+)|(?P<fvalues>".*?")|(?P<fvalueo>.*?)))?;)
#Enum (?P<enum>enum\s*(?P<ename>[A-Za-z0-9_\.]*)\s*\{(?P<econtent>.*?)\}) //dotall
#->enum data: (\s*(?P<evalue>[a-z_]+)\s*(=\s*(?<edefvalue>.*?)\s*)?;)
class IgorTokenizer:
    def __init__(self):
        self.tokenRegexp = re.compile(
            r"""
            (?P<string>"([^"\\]|\\.)*"|"[^"\\]*?$)
            |(?P<number>[0-9]{1,2}\#[0-9a-z]*|[0-9]*\.?[0-9]+)
            |(?P<value>s->c|c->s|[a-zA-Z][a-zA-Z0-9_]*)
            |(?P<bracket>\(|\[|\{|\)|\]|\})
            |(?P<operator>\+|\-|/|\*|<=|>=|==|=|\?|:|\.|\,)
            |(?P<other>\S+)
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
            d = m.groupdict()
            for g in d:
                if d[g] != None:
                    value = d[g]
                    start = m.start(g)
                    end = m.end(g)
                    #print g, value, start, end
                    tokens.append(Token(g, value, start, end))
            pos = m.end()
        return tokens

import pyparsing as pp

L = pp.Literal

any = pp.Regex(r"[\u0000-\uffff]")
alpha = pp.alphas + "_"
digit = pp.nums
NL = L("\r\n") | L("\n") | L("\r")
comment = pp.Suppress(pp.Group(L('//') + pp.ZeroOrMore(pp.NotAny(NL) + any) + NL) |
                      pp.Group(L('/*') + pp.ZeroOrMore(pp.NotAny(L("*/")) + any) + L('*/')))

white_space = L(' ') | L('\t') | NL | comment
WS = pp.ZeroOrMore(white_space) + pp.NotAny(white_space)
SPACE = pp.OneOrMore(white_space) + pp.NotAny(white_space)

name = alpha + pp.ZeroOrMore(alpha + digit)
identifier = name.copy()
quoted_string = L('"') + pp.ZeroOrMore(pp.NotAny('"') + any) + L('"')

type = pp.Forward()
primitive_type = (L("bool") | L("sbyte") | L("byte") | L("short") | L("ushort") | L("int") | L("uint") | L("long") |
                 L("ulong") | L("float") | L("double") | L("string") | L("binary") | L("atom"))
list_type = L("list") + WS + L('[') + WS + type + WS + L(']')
dict_type = L("dict") + WS + L('[') + WS + type + WS + L(',') + WS + type + WS + L(']')
custom_type = identifier.copy()
optional_type = L('?') + WS + type
type << (primitive_type | list_type | dict_type | custom_type | optional_type)

format = L(":") + WS + (L("igor") | L("json"))
side = L("-") + WS + (L("client") | L("server"))

attribute_language = pp.Optional(WS + name + pp.NotAny(alpha + digit) + WS)

#grammar {
#    any = ['\u0000'..'\uFFFF'];
#alpha = ['a'..'z','A'..'Z'] / '_';
#digit = ['0'..'9'];
#NL = "\r\n" / '\r' / '\n';
#comment = ("//" (!NL any)* NL) / ("/*" (!"*/" any)* "*/");
#
#white_space = ' ' / '\t' / NL / comment;
#WS : void = white_space* !white_space;
#SPACE : void = white_space+ !white_space;
#
#name : string = alpha (alpha/digit)*;
#identifier : Identifier = alpha (alpha/digit)*;
#quoted_string : RawValue.String = '"' (!'"' any)* '"';
#
#primitive_type : RawType = "bool" / "sbyte" / "byte" / "short" / "ushort" / "int" / "uint" / "long" / "ulong" / "float" / "double" / "string" / "binary" / "atom";
#list_type : RawType = "list" WS '[' WS type WS ']';
#dict_type : RawType = "dict" WS '[' WS type WS ',' WS type WS ']';
#custom_type : RawType = identifier;
#optional_type : RawType = '?' WS type;
#type : RawType = primitive_type / list_type / dict_type / custom_type / optional_type;
#
#format : Format = ':' WS ("igor" / "json");
#side : Side = '-' WS ("client" / "server");
#
#attribute_language : string = (WS name !(alpha/digit) WS)?;
#attribute_side : option[Side] = (side !(alpha/digit) WS)?;
#attribute_format : option[Format] = (format !(alpha/digit) WS)?;
#attribute_value : RawValue = ('=' WS value WS)?;
#attribute_list : List[Identifier*RawValue] = (WS identifier WS attribute_value)+;
#
#attr : List[Attribute] = '[' attribute_language attribute_side attribute_format attribute_list ']';
#attr2 : List[Attribute] = '[' attribute_side attribute_format attribute_list ']';
#
#attributes : List[Attribute] = ((attr / attr2) WS)*;
#
#enum_field : EnumField = attributes identifier WS ('=' WS int_value WS)? ';';
#enum_form : Form = attributes "enum" SPACE identifier WS '{' WS (enum_field WS)* '}';
#
#inner_comma = &']' / (',' !(WS ']'));
#inner_comma2 = &'}' / (',' !(WS '}'));
#inner_comma3 = &')' / (',' !(WS ')'));
#inner_comma4 = &';' / (',' !(WS ';'));
#inner_comma5 = &'{' / (',' !(WS '{'));
#
#bool_value : RawValue = "true" / "false";
#int_value : RawValue = '-'? digit+ !'.';
#float_value : RawValue = '-'? digit+ '.' + digit+;
#string_value : RawValue = quoted_string;
#identifier_value : RawValue = identifier;
#list_value : RawValue = '[' (WS value WS inner_comma )* WS ']';
#dict_value : RawValue = '[' (WS value WS '=' WS value WS inner_comma)* WS ']';
#record_value : RawValue = '{' (WS identifier WS '=' WS value WS inner_comma2)* WS '}';
#value : RawValue = bool_value / int_value / float_value / string_value / identifier_value / list_value / dict_value / record_value;
#
#record_field : RecordField = attributes type SPACE identifier WS ('=' WS value WS)? ';';
#record_fields : List[RecordField] = '{' WS (record_field WS)* '}';
#record_tag : RawValue = '[' WS value WS ']' WS;
#record_impls : List[Identifier] = ':' (WS identifier WS inner_comma5)+ WS;
#record_form : Form = attributes "record" SPACE (identifier '.')? name WS record_tag? record_impls? record_fields;
#exception_form : Form = attributes "exception" SPACE name WS record_fields;
#variant_form : Form = attributes "variant" SPACE name WS record_impls? record_fields;
#interface_form : Form = attributes "interface" SPACE name WS record_fields;
#
#define_form : Form = attributes "define" SPACE identifier WS type WS ';';
#
#direction : Direction = ('c' WS "->" WS 's') / ('s' WS "->" WS 'c');
#arguments : List[FunctionArgument] = '(' (WS type SPACE identifier WS inner_comma3)* WS ')';
#
#service_function : ServiceFunction = attributes direction SPACE identifier WS arguments WS ("returns" WS arguments WS)? ("throws" SPACE (identifier WS inner_comma4 WS)+)? ';';
#service_form : Form = attributes "service" SPACE identifier WS '{' WS (service_function WS)* '}';
#
#form : Form = enum_form / record_form / exception_form / variant_form / interface_form / define_form / service_form;
#
#mod : Module = attributes "module" SPACE name WS '{' WS (form WS)* '}';
#
#import : Import = attributes "import" SPACE quoted_string WS ';';
#
#file : File = WS (import WS)* (mod WS)* !any;
#})]
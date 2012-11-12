__author__ = 'Yaroslav Nikityshev aka IDNoise'

from idn_token import Token, ErlangTokenType, ErlangTokenizer, IgorTokenizer, IgorTokenType

class ErlangHighlightType:
    STRING, COMMENT, ARROW, VAR, MACROS, ATOM, MODULE, \
    FUNCTION, KEYWORD, MODULEATTR, RECORD, RECORDDEF, NUMBER, \
    FUNDEC, BRACKET, BIF, FULLSTOP, DEFAULT = range(1, 19)

class ErlangHighlighter:
    FUN = "fun"

    RULES = {
        ErlangTokenType.COMMENT: ErlangHighlightType.COMMENT,
        ErlangTokenType.OB: ErlangHighlightType.BRACKET,
        ErlangTokenType.CB: ErlangHighlightType.BRACKET,
        ErlangTokenType.STRING: ErlangHighlightType.STRING,
        ErlangTokenType.RECORD: ErlangHighlightType.RECORD,
        ErlangTokenType.FULLSTOP: ErlangHighlightType.FULLSTOP,
        ErlangTokenType.ARROW: ErlangHighlightType.ARROW,
        ErlangTokenType.NUMBER: ErlangHighlightType.NUMBER,
        ErlangTokenType.MACROS: ErlangHighlightType.MACROS
    }

    KEYWORDS = {"after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case", "catch", "cond",
                "div", "end", "fun", "if", "let", "not", "of", "or", "orelse", "query", "receive", "rem", "try", "when",
                "xor"}

    MODULEATTR = {"-behavior", "-behaviour", "-compile", "-created", "-created_by", "-export", "-file", "-import",
                  "-module", "-modified", "-modified_by", "-record", "-revision", "-spec", "-type", "-vsn", "-extends",
                  "-define", "-else", "-endif", "-ifdef", "-ifndef", "-include", "-include_lib", "-undef", "-export_type",
                  "-spec", "-extends"}

    BIF = {"abs", "adler32", "adler32_combine", "apply", "atom_to_binary", "atom_to_list", "binary_to_atom",
           "binary_to_existing_atom", "binary_to_list", "bitstring_to_list", "binary_to_term", "bit_size", "byte_size",
           "check_process_code", "concat_binary", "crc32", "crc32_combine", "date", "decode_packet", "delete_module",
           "disconnect_node", "element", "erase", "exit", "float", "float_to_list", "garbage_collect", "get", "get_keys",
           "group_leader", "halt", "hd", "integer_to_list", "iolist_to_binary", "iolist_size", "is_alive", "is_atom",
           "is_binary", "is_bitstring", "is_boolean", "is_float", "is_function", "is_integer", "is_list", "is_number",
           "is_pid", "is_port", "is_process_alive", "is_record", "is_reference", "is_tuple", "length", "link",
           "list_to_atom", "list_to_binary", "list_to_bitstring", "list_to_existing_atom", "list_to_float",
           "list_to_integer", "list_to_pid", "list_to_tuple", "load_module", "make_ref", "module_loaded", "monitor_node",
           "node", "nodes", "now", "open_port", "pid_to_list", "port_close", "port_command", "port_connect",
           "port_control", "pre_loaded", "process_flag", "process_info", "processes", "purge_module", "put", "register",
           "registered", "round", "self", "setelement", "size", "spawn", "spawn_link", "spawn_monitor", "spawn_opt",
           "split_binary", "statistics", "term_to_binary", "throw", "time", "tl", "trunc", "tuple_size", "tuple_to_list",
          "unlink", "unregister", "whereis"}

    DOCS = {"@author", "@clear", "@copyright", "@deprecated", "@doc", "@docfile", "@end", "@equiv", "@headerfile",
            "@hidden", "@private", "@reference", "@see", "@since", "@spec", "@throws", "@title", "@todo", "@TODO",
            "@type", "@version", "@date", "@docRoot", "@link", "@module", "@package", "@section", "@time", "@type",
            "@version"}

    def __init__(self):
        self.tokenizer = ErlangTokenizer()

    def GetHighlightingTokens(self, text):
        result = []
        tokens = self.tokenizer.GetTokens(text)
        if not tokens: return result
        firstToken = tokens[0]
        for i in range(len(tokens)):
            token = tokens[i]
            if token.type == ErlangTokenType.SPACE:
                continue
            tokenType = ErlangHighlightType.DEFAULT
            if token.type in self.RULES:
                tokenType = self.RULES[token.type]

            elif token.type ==  ErlangTokenType.FUNDEC:
                result.append(Token(ErlangHighlightType.FUNDEC, token.value[:-1], token.start, token.end - 1, ))
                result.append(Token(ErlangHighlightType.BRACKET, token.value[-1], token.end - 1, token.end))
                continue

            elif token.type == ErlangTokenType.VAR:
                if i == 2 and firstToken.value == "-define":
                    tokenType = ErlangHighlightType.MACROS
                else:
                    tokenType = ErlangHighlightType.VAR

            elif token.type == ErlangTokenType.MODULEATTR:
                tokenType = ErlangHighlightType.MODULEATTR
                #if token.value in self.MODULEATTR:


            elif token.type == ErlangTokenType.ATOM:
                if token.value in self.KEYWORDS:
                    tokenType = ErlangHighlightType.KEYWORD

                elif i == 2 and firstToken.value in  ["-module", "-extends"]:
                    tokenType = ErlangHighlightType.MODULE
                elif i == 2 and firstToken.value == "-record":
                    tokenType = ErlangHighlightType.RECORDDEF
                elif i == 2 and firstToken.value == "-define":
                    tokenType = ErlangHighlightType.MACROS

                elif (i + 1 < len(tokens) and tokens[i + 1].value == ":" and
                      token.value not in ["throw", "error", "exit"] and
                      not (len(tokens) > i + 2 and tokens[i + 2].value == ":")):
                    tokenType = ErlangHighlightType.MODULE

                elif i + 1 < len(tokens) and tokens[i + 1].value == "(":
                    #if token.value in self.BIF: tokenType = ErlangHighlightType.BIF
                    #else: tokenType = ErlangHighlightType.FUNCTION
                    tokenType = ErlangHighlightType.FUNCTION

                elif ((i - 2 >= 0 and tokens[i - 2].value == self.FUN
                       and tokens[i - 1].type == ErlangTokenType.SPACE) or
                      (i - 4 >= 0 and tokens[i - 4].value == self.FUN
                       and tokens[i - 1].value == ":" and tokens[i - 2].type == ErlangTokenType.ATOM)):
                    tokenType = ErlangHighlightType.FUNCTION

                elif (i + 2 < len(tokens) and tokens[i + 1].value == "/"):
                    tokenType = ErlangHighlightType.FUNCTION

                else: tokenType = ErlangHighlightType.ATOM
            token.type = tokenType
            result.append(token)

        return result

class IgorHighlightType:
    DEFAULT, STRING, NUMBER, KEYWORD, TYPE, VALUE, SPECIAL_SYMBOL = range(1, 8)

class IgorHighlighter:

    keywords = ["enum", "record", "variant", "service",
                "module", "ethereal", "csharp", "erlang",
                "import", "s->c", "c->s", "define",
                "schema", "list", "dict", "tag"]

    RULES = {
        IgorTokenType.STRING: IgorHighlightType.STRING,
        IgorTokenType.NUMBER: IgorHighlightType.NUMBER,
        #IgorTokenType.TYPE: IgorHighlightType.TYPE,
        IgorTokenType.VALUE: IgorHighlightType.VALUE
    }

    def __init__(self):
        self.tokenizer = IgorTokenizer()

    #define FieldId int;
#    c->s Move(Action action);
#    c->s Cast(Key spell_key, SpellTarget target);
#    s->c AddUnit(Tick tick, UnitInfo info, Frame frame, bool is_me);


    def GetHighlightingTokens(self, text):
        result = []
        tokens = self.tokenizer.GetTokens(text)
        if not tokens: return result
        firstToken = tokens[0]
        if firstToken.type == IgorTokenType.SPACE and len(tokens) > 1:
            firstToken = tokens[1]
        for i in range(len(tokens)):
            token = tokens[:][i]

            tokenType = IgorHighlightType.DEFAULT
            if token.value in self.keywords:
                tokenType = IgorHighlightType.KEYWORD
            elif token.value in ["?"]:
                tokenType = IgorHighlightType.SPECIAL_SYMBOL
            elif token.type == IgorTokenType.VALUE:
                if firstToken.value in ["c->s", "s->c"]:
                    checkToken = tokens[i - 1]
                    if checkToken.type == IgorTokenType.SPACE:
                        checkToken = tokens[i - 2]
                    if checkToken.value in ["(", ","]:
                        tokenType = IgorHighlightType.TYPE
                    else:
                        tokenType = IgorHighlightType.VALUE
                elif firstToken.value == "define":
                    if tokens[i + 1].value == ";":
                        tokenType = IgorHighlightType.TYPE
                    else:
                        tokenType = IgorHighlightType.VALUE
                elif (firstToken.value in ["record", "module", "enum", "variant", "service"] and
                    tokens[i - 1].value != "["):
                    tokenType = IgorHighlightType.TYPE
                elif firstToken == token and tokens[i + 1].value == ";":
                    tokenType = IgorHighlightType.VALUE
                elif tokens[i - 1].value == "[" and tokens[i + 1].value == "]":
                    tokenType = IgorHighlightType.VALUE
                else:
                    j = i
                    checkToken = tokens[j + 1]
                    while checkToken.type == IgorTokenType.SPACE and len(tokens) > j:
                        j += 1
                        checkToken = tokens[j + 1]
                    if checkToken.value in ["=", ";"]:
                        tokenType = IgorHighlightType.VALUE
                    else:
                        tokenType = IgorHighlightType.TYPE

            elif token.type in self.RULES:
                tokenType = self.RULES[token.type]
            result.append(Token(tokenType, token.value, token.start, token.end))

        return result
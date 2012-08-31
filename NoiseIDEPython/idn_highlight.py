__author__ = 'Yaroslav Nikityshev aka IDNoise'

from idn_token import Token, ErlangTokenType, ErlangTokenizer

class ErlangHighlightType:
    STRING, COMMENT, ARROW, VAR, MACROS, ATOM, MODULE, SPEC, \
    FUNCTION, KEYWORD, MODULEATTR, PREPROC, RECORD, RECORDDEF, NUMBER, \
    FUNDEC, BRACKET, BIF, FULLSTOP, DEFAULT = range(1, 21)

class ErlangHighlighter:
    DEFINE = "-define"
    MODULE = "-module"
    EXTENDS = "-extends"
    FUN = "fun"
    RECORD = "-record"
    SPEC = "-spec"

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

    KEYWORDS = {"after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case", "catch", "cond"
        , "div", "end", "fun", "if", "let", "not", "of", "or", "orelse", "query", "receive", "rem", "try", "when",
                "xor"}

    MODULEATTR = {"-behavior", "-behaviour", "-compile", "-created", "-created_by", "-export", "-file", "-import",
                  "-module", "-modified", "-modified_by", "-record", "-revision", "-spec", "-type", "-vsn", "-extends"}

    PREPROC = {"-define", "-else", "-endif", "-ifdef", "-ifndef", "-include", "-include_lib", "-undef", "-export_type"}

    BIF = {"abs", "adler32", "adler32_combine", "apply", "atom_to_binary", "atom_to_list", "binary_to_atom",
           "binary_to_existing_atom", "binary_to_list", "bitstring_to_list", "binary_to_term", "bit_size", "byte_size",
           "check_process_code", "concat_binary", "crc32", "crc32_combine", "date", "decode_packet", "delete_module",
           "disconnect_node", "element", "erase", "exit", "float", "float_to_list", "garbage_collect", "get", "get_keys"
        , "group_leader", "halt", "hd", "integer_to_list", "iolist_to_binary", "iolist_size", "is_alive", "is_atom",
           "is_binary", "is_bitstring", "is_boolean", "is_float", "is_function", "is_integer", "is_list", "is_number",
           "is_pid", "is_port", "is_process_alive", "is_record", "is_reference", "is_tuple", "length", "link",
           "list_to_atom", "list_to_binary", "list_to_bitstring", "list_to_existing_atom", "list_to_float",
           "list_to_integer", "list_to_pid", "list_to_tuple", "load_module", "make_ref", "module_loaded", "monitor_node"
        , "node", "nodes", "now", "open_port", "pid_to_list", "port_close", "port_command", "port_connect",
           "port_control", "pre_loaded", "process_flag", "process_info", "processes", "purge_module", "put", "register",
           "registered", "round", "self", "setelement", "size", "spawn", "spawn_link", "spawn_monitor", "spawn_opt",
           "split_binary", "statistics", "term_to_binary", "throw", "time", "tl", "trunc", "tuple_size", "tuple_to_list"
        , "unlink", "unregister", "whereis"}

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
                if i == 2 and firstToken.value == self.DEFINE:
                    tokenType = ErlangHighlightType.MACROS
                else:
                    tokenType = ErlangHighlightType.VAR

            elif token.type == ErlangTokenType.PREPROC:
                if token.value == self.SPEC:
                    tokenType = ErlangHighlightType.SPEC
                elif token.value in self.MODULEATTR:
                    tokenType = ErlangHighlightType.MODULEATTR
                elif token.value in self.PREPROC:
                    tokenType = ErlangHighlightType.PREPROC

            elif token.type == ErlangTokenType.ATOM:
                if token.value in self.KEYWORDS:
                    tokenType = ErlangHighlightType.KEYWORD

                elif i == 2 and firstToken.value in  [self.MODULE, self.EXTENDS]:
                    tokenType = ErlangHighlightType.MODULE
                elif i == 2 and firstToken.value == self.RECORD:
                    tokenType = ErlangHighlightType.RECORDDEF
                elif i == 2 and firstToken.value == self.DEFINE:
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
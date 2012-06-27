require "set"
require_relative "tokenizers.rb"

class HighlightData
  attr_accessor :type
  attr_accessor :value
  attr_accessor :start_pos
  attr_accessor :end_pos

  def initialize(type, value, start_pos, end_pos)
    @type = type
    @value = value
    @start_pos = start_pos
    @end_pos = end_pos
  end
end

class ErlangHighlightType
  STRING, COMMENT, ARROW, VARIABLE, MACROS, ATOM, MODULE, SPEC,
  FUNCTION, KEYWORD, MODULEATTR, PREPROC, RECORD, RECORDDEF, NUMBER,
  FUNDEC, BRACKET, BIF, FULLSTOP, DEFAULT = (1..20).to_a
end

class BaseHighlighter
  def get_highlighting_tokens(line_text)
    raise(NotImplementedError)
  end
end

class ErlangHighlighter < BaseHighlighter
  DEFINE = "-define"
  MODULE = "-module"
  EXTENDS = "-extends"
  FUN = "fun"
  RECORD = "-record"
  SPEC = "-spec"
  def initialize(tokenizer)
    @tokenizer = tokenizer

    @rules = Hash.new
    @rules[ErlangTokenizer::COMMENT] = ErlangHighlightType::COMMENT
    @rules[ErlangTokenizer::OB] = ErlangHighlightType::BRACKET
    @rules[ErlangTokenizer::CB] = ErlangHighlightType::BRACKET
    @rules[ErlangTokenizer::STRING] = ErlangHighlightType::STRING
    @rules[ErlangTokenizer::RECORD] = ErlangHighlightType::RECORD
    @rules[ErlangTokenizer::FULLSTOP] = ErlangHighlightType::FULLSTOP
    @rules[ErlangTokenizer::ARROW] = ErlangHighlightType::ARROW
    @rules[ErlangTokenizer::NUMBER] = ErlangHighlightType::NUMBER
    @rules[ErlangTokenizer::MACROS] = ErlangHighlightType::MACROS
  end
  def get_highlighting_tokens(line_text)
    tokens = @tokenizer.get_tokens(line_text)
    result = []
    return result if not tokens
    first_token = tokens[0]
    tokens.each_index { |i|
      token = tokens[i]
      if token.type == ErlangTokenizer::SPACE
        next
      end
      type = ErlangHighlightType::DEFAULT
      if @rules.has_key? token.type
        type = @rules[token.type]
      else
        case token.type
          when ErlangTokenizer::FUNDEC then
            result << Token.new(ErlangHighlightType::FUNDEC, token.value[0..-2], token.start_pos, token.end_pos - 1)
            result << Token.new(ErlangHighlightType::BRACKET, token.value[-1], token.end_pos - 1, token.end_pos)
            next
          when ErlangTokenizer::VARIABLE then
            if i == 2 and first_token.value == DEFINE
              type = ErlangHighlightType::MACROS
            else
              type = ErlangHighlightType::VARIABLE
            end
          when ErlangTokenizer::PREPROC then
            if token.value == SPEC
              type = ErlangHighlightType::SPEC
            elsif ErlangSyntaxData::MODULEATTRS.member? token.value
              type = ErlangHighlightType::MODULEATTR
            elsif ErlangSyntaxData::PREPROC.member? token.value
              type = ErlangHighlightType::PREPROC
            end
          when ErlangTokenizer::ATOM then
            if ErlangSyntaxData::KEYWORDS.member? token.value
              type = ErlangHighlightType::KEYWORD
            elsif i == 2 and [MODULE, EXTENDS].member? first_token.value
              type = ErlangHighlightType::MODULE
            elsif i == 2 and first_token.value == RECORD
              type = ErlangHighlightType::RECORDDEF
            elsif i == 2 and first_token.value == DEFINE
              type = ErlangHighlightType::MACROS
            elsif (i + 1 < tokens.length and tokens[i + 1].value == ":" and
              not ["throw", "error", "exit"].member? token.value and
                not (tokens.length > i + 2 and tokens[i + 2].value == ":"))
              type = ErlangHighlightType::MODULE
            elsif i + 1 < tokens.length and tokens[i + 1].value == "("
              if ErlangSyntaxData::BIFS.member? token.value
                type = ErlangHighlightType::BIF
              else
                type = ErlangHighlightType::FUNCTION
              end
            elsif ((i - 2 >= 0 and tokens[i - 2].value == FUN and
                    tokens[i-1].type == ErlangTokenizer::SPACE) or
                   (i - 4 >= 0 and tokens[i - 4].value == FUN and
                   tokens[i - 1].value == ":" and tokens[i - 2].type == ErlangTokenizer::ATOM))
              type = ErlangHighlightType::FUNCTION
            else
              type = ErlangHighlightType::ATOM
            end
        end
      end
      result << Token.new(type, token.value, token.start_pos, token.end_pos)
    }
    result
  end
end

class ErlangSyntaxData
   KEYWORDS = [
      "after",
      "and",
      "andalso",
      "band",
      "begin",
      "bnot",
      "bor",
      "bsl",
      "bsr",
      "bxor",
      "case",
      "catch",
      "cond",
      "div",
      "end",
      "fun",
      "if",
      "let",
      "not",
      "of",
      "or",
      "orelse",
      "query",
      "receive",
      "rem",
      "try",
      "when",
      "xor"
  ].to_set

  MODULEATTRS = [
      "-behavior",
      "-behaviour",
      "-compile",
      "-created",
      "-created_by",
      "-export",
      "-file",
      "-import",
      "-module",
      "-modified",
      "-modified_by",
      "-record",
      "-revision",
      "-spec",
      "-type",
      "-vsn",
      "-extends"
  ].to_set

  PREPROC = [
      "-define",
      "-else",
      "-endif",
      "-ifdef",
      "-ifndef",
      "-include",
      "-include_lib",
      "-undef",
      "-export_type"
  ].to_set

  BIFS = [
      "abs",
      "adler32",
      "adler32_combine",
      "apply",
      "atom_to_binary",
      "atom_to_list",
      "binary_to_atom",
      "binary_to_existing_atom",
      "binary_to_list",
      "bitstring_to_list",
      "binary_to_term",
      "bit_size",
      "byte_size",
      "check_process_code",
      "concat_binary",
      "crc32",
      "crc32_combine",
      "date",
      "decode_packet",
      "delete_module",
      "disconnect_node",
      "element",
      "erase",
      "exit",
      "float",
      "float_to_list",
      "garbage_collect",
      "get",
      "get_keys",
      "group_leader",
      "halt",
      "hd",
      "integer_to_list",
      "iolist_to_binary",
      "iolist_size",
      "is_alive",
      "is_atom",
      "is_binary",
      "is_bitstring",
      "is_boolean",
      "is_float",
      "is_function",
      "is_integer",
      "is_list",
      "is_number",
      "is_pid",
      "is_port",
      "is_process_alive",
      "is_record",
      "is_reference",
      "is_tuple",
      "length",
      "link",
      "list_to_atom",
      "list_to_binary",
      "list_to_bitstring",
      "list_to_existing_atom",
      "list_to_float",
      "list_to_integer",
      "list_to_pid",
      "list_to_tuple",
      "load_module",
      "make_ref",
      "module_loaded",
      "monitor_node",
      "node",
      "nodes",
      "now",
      "open_port",
      "pid_to_list",
      "port_close",
      "port_command",
      "port_connect",
      "port_control",
      "pre_loaded",
      "process_flag",
      "process_info",
      "processes",
      "purge_module",
      "put",
      "register",
      "registered",
      "round",
      "self",
      "setelement",
      "size",
      "spawn",
      "spawn_link",
      "spawn_monitor",
      "spawn_opt",
      "split_binary",
      "statistics",
      "term_to_binary",
      "throw",
      "time",
      "tl",
      "trunc",
      "tuple_size",
      "tuple_to_list",
      "unlink",
      "unregister",
      "whereis"
  ].to_set

  DOCS = [
      "@author",
      "@clear",
      "@copyright",
      "@deprecated",
      "@doc",
      "@docfile",
      "@end",
      "@equiv",
      "@headerfile",
      "@hidden",
      "@private",
      "@reference",
      "@see",
      "@since",
      "@spec",
      "@throws",
      "@title",
      "@todo",
      "@TODO",
      "@type",
      "@version",
      "@date",
      "@docRoot",
      "@link",
      "@module",
      "@package",
      "@section",
      "@time",
      "@type",
      "@version"
  ].to_set
end

#t = ErlangHighlighter.new(ErlangTokenizer.new)
#puts t.get_highlighting_tokens("-define(X, xx).")
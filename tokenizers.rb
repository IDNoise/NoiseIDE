class Token
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

  def inspect
    "Type: #{@type}, Value: #{@value}, Start: #{@start_pos}, End: #{@end_pos}"
  end

  def to_s
    inspect
  end
end

class BaseTokenizer
  def get_tokens(text)
    raise(NotImplementedError)
  end
end

class ErlangTokenizer < BaseTokenizer
  COMMENT = "comment"
  ARROW = "arrow"
  STRING = "string"
  FUNDEC = "fundec"
  VARIABLE = "var"
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

  def initialize
    @erlang_token_regexp = Regexp::compile(%r{
      (?<#{COMMENT}>%.*$)|
      (?<#{ARROW}>->) |
      (?<#{STRING}>"([^"\\]|\.)*")|
      (?<#{FUNDEC}>^(?!fun\()[a-z][a-zA-Z_0-9]*\() |
      (?<#{VARIABLE}>[A-Z_][a-zA-Z_0-9]*)|
      (?<#{PREPROC}>^-[a-z][a-z_]*) |
      (?<#{RECORD}>\#[a-z][a-z_]*) |
      (?<#{NUMBER}>[0-9]*\.?[0-9]+) |
      (?<#{MACROS}>\?[a-zA-Z][a-zA-Z_0-9]*)|
      (?<#{ATOM}>'?[a-zA-Z_0-9]+'?|[a-z][a-zA-Z_0-9]*)|
      (?<#{OB}>\(|\[|{) |
      (?<#{CB}>\)|\]|})|
      (?<#{OPERATOR}>\+|\-|\/|\*|<=|=<|>=|==|=\/=|=:=|<-|!)|
      (?<#{FULLSTOP}>\s*\.\s*$)|
      (?<#{OTHER}>\S)|
      (?<#{SPACE}>\s+)
      /}mx
    )

  end

  def get_tokens(text)
    matches = text.to_enum(:scan, @erlang_token_regexp).map { Regexp.last_match }
    tokens =
      matches.map {  |m|
        start_pos = m.begin(0)
        end_pos = m.end(0)
        type = m.names.select{|n| not m[n].nil? }[0]
        value = m[type]
        #puts [type, value, start_pos, end_pos]
        Token.new(type, value, start_pos, end_pos)
      }
    tokens
  end
end

#t = ErlangTokenizer.new
#puts t.get_tokens("-define(X, xx).")


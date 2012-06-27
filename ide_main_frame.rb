require "wx"
require_relative "custom_stc.rb"
require_relative "lexers.rb"
require_relative "colorschema.rb"

include Wx

class IdeMainFrame < Frame
  def initialize
    super(nil,
          :pos => [150, 25],
          :size => [800, 900],
          :title => "IDNoise IDE")
    ColorSchema.load("dark")
    tokenizer = ErlangTokenizer.new
    highlighter = ErlangHighlighter.new(tokenizer)
    lexer = ErlangLexer.new(highlighter)
    CustomSTC.new(self, lexer, File.dirname(__FILE__) + "/eide_cache.erl")
  end
end
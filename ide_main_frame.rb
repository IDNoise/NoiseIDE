require "wx"
require_relative "custom_stc.rb"
require_relative "colorschema.rb"

include Wx


class IdeMainFrame < Frame
  def initialize
    super(nil,
          :pos => [150, 25],
          :size => [800, 900],
          :title => "IDNoise IDE")
    ColorSchema.load("dark")

    @notebook = Notebook.new(self, :style => NB_TOP | NB_MULTILINE)
    add_test_tabs
    #editor = ErlangSTC.new(@notebook, File.dirname(__FILE__) + "/eide_cache.erl")
    #@notebook.add_page(editor, editor.file_name())
    #
  end

  def add_test_tabs
    (1..15).each {|i|
      editor = ErlangSTC.new(@notebook, File.dirname(__FILE__) + "/eide_cache.erl")
      @notebook.add_page(editor, editor.file_name() + i.to_s)
    }
  end
end
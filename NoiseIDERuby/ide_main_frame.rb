require "wx"
require_relative "custom_stc.rb"
require_relative "colorschema.rb"

include Wx


class IdeMainFrame < Frame
  def initialize
    super(nil,
          :pos => [150, 25],
          :size => [1680, 1050],
          :title => "IDNoise IDE")

    ColorSchema.load("dark")

    @mgr = AuiManager.new(self)

    @explorer = TreeCtrl.new(self, :size => [300, 500])
    explorer_info = AuiPaneInfo.new
    explorer_info.set_maximize_button()
    explorer_info.set_minimize_button(false)
    explorer_info.set_minimize_button(true)
    explorer_info.left
    explorer_info.set_caption("Explorer")
    explorer_info.set_close_button(false)
    explorer_info.set_best_size(300, 600)
    @mgr.add_pane(@explorer, explorer_info)

    @tabmgr = AuiNotebook.new(self, ID_ANY, DEFAULT_POSITION, DEFAULT_SIZE, AUI_NB_DEFAULT_STYLE | AUI_NB_CLOSE_ON_ALL_TABS)
    tabmgr_info = AuiPaneInfo.new
    tabmgr_info.set_maximize_button()
    tabmgr_info.set_minimize_button()
    tabmgr_info.center
    tabmgr_info.set_caption("Tab Manager")
    tabmgr_info.set_close_button(false)
    @mgr.add_pane(@tabmgr, tabmgr_info)

    @mgr.update()

    add_test_tabs
  end

  def add_test_tabs
    (1..15).each {|i|
      editor = ErlangSTC.new(@tabmgr, File.dirname(__FILE__) + "/eide_cache.erl")
      @tabmgr.add_page(editor, editor.file_name() + i.to_s)
    }
  end
end
require "wx"
require_relative "lexers.rb"
include Wx



class String
  def to_color
    Colour.new(self[1..2].to_i(16), self[3..4].to_i(16), self[5..6].to_i(16))
  end
end

module EditorFoldMixin
  def init_editor_fold_mixin
    evt_stc_marginclick self, :on_margin_click
    evt_stc_change self, :on_text_change
  end

  def on_margin_click(event)
    line_num = self.line_from_position(event.get_position)
    if(event.get_margin == 2)
      toggle_fold(line_num)
    end
    event.skip()
  end

  def on_text_change(event)
    line = line_from_position(get_current_pos())
    if (get_fold_level(line) & STC_FOLDLEVELHEADERFLAG and
         not get_fold_expanded(line))
        toggle_fold(line)
      event.skip()
    end
  end
end

module EditorLineMarginMixin
  def init_editor_line_margin_mixin
    #evt_stc_updateui self, :on_updated
    evt_stc_change self, :on_updated
    evt_stc_zoom self, :on_zoom
  end

  def calc_font_width()
    dc = WindowDC.new(self)
    font = get_default_font()
    default_font_size = font.get_point_size()
    font.set_point_size(default_font_size + get_zoom())
    dc.set_font(font)
    default_font_width, height = dc.get_text_extent("9")
    #puts( @default_font_width)
    font.set_point_size(default_font_size)
    default_font_width
  end

  def on_updated(event)
    if @number_area_enabled
      enable_line_number_margin()
    end
    event.skip()
  end

  def on_zoom(event)
    if @number_area_enabled
      enable_line_number_margin()
    end
    event.skip()
  end

  def enable_line_number_margin(enable = true)
    @number_area_enabled = enable
    #puts enable
    if enable
      width = 6 + get_line_count().to_s.length * calc_font_width
      set_margin_width(1, width)
    else
      set_margin_width(1, 0)
    end
  end
end

class CustomSTC < StyledTextCtrl

  include EditorFoldMixin
  include EditorLineMarginMixin

  attr_accessor :file_path
  attr_accessor :lexer

  def initialize(frame, lexer, file_path)
    super(frame)

    init_editor_fold_mixin
    init_editor_line_margin_mixin

    @file_path = file_path
    @lexer = lexer
    set_lexer(STC_LEX_CONTAINER)
    #set_lexer(STC_LEX_RUBY)

    set_caret_width(3)
    set_caret_line_background(ColorSchema.code_editor["current_line_background"].to_color)
    set_caret_line_visible(true)

    set_tab_width(2)
    set_use_tabs(false)
    set_tab_indents(true)
    set_back_space_un_indents(true)
    set_indent(2)
    set_edge_column(140)
    set_edge_mode(STC_EDGE_LINE)

    set_margins(5, 5)
    set_property("fold", "1")
    #set_property("fold.compact", "0")
    #set_property("fold.comment", "1")
    #set_property("fold.preprocessor", "1")

    #Line number margin
    set_margin_type(1, STC_MARGIN_NUMBER)
    set_margin_mask(1, 0)
    enable_line_number_margin
    #Fold Margin
    set_margin_type(2, STC_MARGIN_SYMBOL)
    set_margin_mask(2, STC_MASK_FOLDERS)
    set_margin_sensitive(2, true)
    set_margin_width(2, 12)

    fore_color = ColorSchema.code_editor["fold_area_foreground"].to_color
    back_color = ColorSchema.code_editor["fold_area_background"].to_color
    marker_define(STC_MARKNUM_FOLDEROPEN, STC_MARK_BOXMINUS, fore_color, back_color)
    marker_define(STC_MARKNUM_FOLDER, STC_MARK_BOXPLUS, fore_color, back_color)
    marker_define(STC_MARKNUM_FOLDERSUB, STC_MARK_VLINE, fore_color, back_color)
    marker_define(STC_MARKNUM_FOLDERTAIL, STC_MARK_LCORNER, fore_color, back_color)
    marker_define(STC_MARKNUM_FOLDEREND, STC_MARK_BOXPLUSCONNECTED, fore_color, back_color)
    marker_define(STC_MARKNUM_FOLDEROPENMID, STC_MARK_BOXMINUSCONNECTED, fore_color, back_color)
    marker_define(STC_MARKNUM_FOLDERMIDTAIL, STC_MARK_TCORNER, fore_color, back_color)
    set_fold_margin_colour(true, back_color)
    set_fold_margin_hi_colour(true, back_color)
    set_fold_flags(16)

    self.font = Font.new(ColorSchema.code_editor["font_size"], FONTFAMILY_DEFAULT,
                         FONTSTYLE_NORMAL, FONTWEIGHT_NORMAL, false,
                         ColorSchema.code_editor["font_name"])
    #styles
    style_set_background(STC_STYLE_DEFAULT, ColorSchema.code_editor["background"].to_color)
    style_set_foreground(STC_STYLE_DEFAULT, ColorSchema.code_editor["foreground"].to_color)
    style_set_font(STC_STYLE_DEFAULT, self.font)
    style_clear_all()
    style_set_background(STC_STYLE_LINENUMBER, ColorSchema.code_editor["line_number_area_background"].to_color)
    style_set_spec(ErlangHighlightType::DEFAULT, ColorSchema.code_formats["default"])
    style_set_spec(ErlangHighlightType::STRING, ColorSchema.code_formats["string"])
    style_set_spec(ErlangHighlightType::COMMENT, ColorSchema.code_formats["comment"])
    style_set_spec(ErlangHighlightType::ARROW, ColorSchema.code_formats["arrow"])
    style_set_spec(ErlangHighlightType::VARIABLE, ColorSchema.code_formats["variable"])
    style_set_spec(ErlangHighlightType::MACROS, ColorSchema.code_formats["macros"])
    style_set_spec(ErlangHighlightType::ATOM, ColorSchema.code_formats["atom"])
    style_set_spec(ErlangHighlightType::MODULE, ColorSchema.code_formats["module"])
    style_set_spec(ErlangHighlightType::SPEC, ColorSchema.code_formats["preproc"])
    style_set_spec(ErlangHighlightType::FUNCTION, ColorSchema.code_formats["function"])
    style_set_spec(ErlangHighlightType::KEYWORD, ColorSchema.code_formats["keyword"])
    style_set_spec(ErlangHighlightType::MODULEATTR, ColorSchema.code_formats["moduleattr"])
    style_set_spec(ErlangHighlightType::PREPROC, ColorSchema.code_formats["preproc"])
    style_set_spec(ErlangHighlightType::RECORD, ColorSchema.code_formats["record"])
    style_set_spec(ErlangHighlightType::RECORDDEF, ColorSchema.code_formats["record"])
    style_set_spec(ErlangHighlightType::NUMBER, ColorSchema.code_formats["number"])
    style_set_spec(ErlangHighlightType::FUNDEC, ColorSchema.code_formats["fundec"])
    style_set_spec(ErlangHighlightType::BRACKET, ColorSchema.code_formats["bracket"])
    style_set_spec(ErlangHighlightType::BIF, ColorSchema.code_formats["bif"])
    style_set_spec(ErlangHighlightType::FULLSTOP, ColorSchema.code_formats["fullstop"])

    style_set_background(STC_STYLE_BRACELIGHT, ColorSchema.code_editor["brace_background"].to_color)
    style_set_foreground(STC_STYLE_BRACELIGHT, ColorSchema.code_editor["brace_foreground"].to_color)
    style_set_background(STC_STYLE_BRACEBAD, ColorSchema.code_editor["brace_bad_background"].to_color)
    style_set_foreground(STC_STYLE_BRACEBAD, ColorSchema.code_editor["brace_bad_foreground"].to_color)

    evt_stc_styleneeded self, :on_style_needed
    evt_stc_updateui self, :highlight_braces

    load_file(@file_path)
  end

  def file_name
    File.basename(@file_path)
  end

  def get_default_font
    font
  end

  def on_style_needed(event)
    @lexer.style_event(event)
    event.skip()
  end

  def highlight_braces(event)
    event.skip()
    pos = get_current_pos()
    char = get_char_at(pos).chr
    if not "()[]{}<>".include? char
      brace_bad_light(-1)
    else
     other_pos = brace_match(pos)
     if other_pos > 0
       brace_highlight(pos, other_pos)
     else
       brace_bad_light(pos)
     end
    end
  end
end

class ErlangSTC < CustomSTC
  @@tokenizer = ErlangTokenizer.new
  @@highlighter = ErlangHighlighter.new(    @@tokenizer)

  def initialize frame, file_path
    lexer = ErlangLexer.new(@@highlighter)
    super frame, lexer, file_path
  end
end
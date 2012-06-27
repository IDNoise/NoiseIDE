require "wx"
require_relative "highlighters.rb"
include Wx

class BaseLexer
  def style_text(control, start_line, end_line)
    raise(NotImplementedError)
  end

  def style_event(event)
    control = event.get_event_object()
    start_pos = control.get_end_styled()
    end_pos = event.get_position()
    style_text(control, start_pos, end_pos)
  end
end

class ErlangLexer < BaseLexer
  def initialize(highlighter)
    @highlighter = highlighter
  end

  def style_text(control, start_pos, end_pos)
    start = start_pos
    line = control.line_from_position(start)
    line_start = control.position_from_line(line)
    end_line = control.line_from_position(end_pos)
    end_line_end_pos = control.get_line_end_position(end_line)
    prev_fold_level = control.get_fold_level(line - 1)
    if prev_fold_level == 1 | STC_FOLDLEVELHEADERFLAG
      prev_fold_level = 2
    elsif prev_fold_level == 3
      prev_fold_level = 0
    end
    next_line_fold_level = prev_fold_level
    # Get Styling Range
    control.start_styling(line_start, 0x1f)
    last_end = line_start
    default_style = ErlangHighlightType::DEFAULT

    while line <= end_line
      current_line_fold_level = next_line_fold_level
      line_end = control.get_line_end_position(line)
      text = control.get_text_range(line_start, line_end)

      @highlighter.get_highlighting_tokens(text).each {|rule|
        if [ErlangHighlightType::FUNDEC,
            ErlangHighlightType::RECORDDEF,
            ErlangHighlightType::SPEC].member? rule.type
          current_line_fold_level = 1
          next_line_fold_level = 2
        elsif rule.type == ErlangHighlightType::FULLSTOP
          #puts "Fullstop " + text
          current_line_fold_level = 3
          next_line_fold_level = 0
        end
        start = line_start + rule.start_pos
        control.set_styling(start - last_end, default_style) if start > last_end
        control.set_styling(rule.value.length, rule.type)
        last_end  = line_start + rule.end_pos
      }
      current_line_fold_level |= STC_FOLDLEVELHEADERFLAG if current_line_fold_level == 1
      control.set_fold_level(line, current_line_fold_level)
      line += 1
      line_start = control.position_from_line(line)
    end
    control.set_styling(end_line_end_pos - last_end, default_style) if last_end < end_line_end_pos
  end
end
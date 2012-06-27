require 'rubygems'
require "wx"
require_relative  "ide_main_frame.rb"
include Wx

class MainApp < App
  def on_init
    @frame = IdeMainFrame.new()
    @frame.show(true)
  end
end

app = MainApp.new
app.main_loop()
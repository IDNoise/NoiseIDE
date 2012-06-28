require 'yaml'

class ColorSchema

  class << self
    attr_accessor :code_editor
    attr_accessor :code_formats
  end


  def ColorSchema.load(schema)
    @schema_file = YAML.load_file(File.dirname(__FILE__) + "/#{schema}.color.yaml")
    #puts @@schema_file
    @code_editor = @schema_file["code_editor"]
    @code_formats = @code_editor["formats"]
    #puts @@code_formats
    #puts @code_editor
    #puts @code_formats
  end
end
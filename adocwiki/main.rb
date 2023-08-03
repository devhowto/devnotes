# rubocop:disable all

require 'yaml'
require 'fileutils'
require 'asciidoctor'
require 'erb'

class AdocWiki
  def initialize(nav_file_path)
    @nav_items = YAML.load_file(nav_file_path)
  end

  def conv(file)
    rhtml = ERB.new(File.read('./adocwiki/templates/page.html.erb', mode: 'r:utf-8'))

    arr = file.split('/')

    # Drop last element, the file with the .adoc extension.
    dirs = arr[0 .. -2]

    # Prepend ‘build’ to the path.
    dirs.insert(0, 'build')

    outname = arr[-1]

    FileUtils.mkpath(dirs.join('/'))

    adoc = Asciidoctor.load_file(
      file,
    )

    html_page = rhtml.result(binding)

    File.write(
      "#{dirs.join('/')}/#{outname.gsub(/adoc$/, 'html')}",
      html_page,
    )

    p adoc.attributes['stem']
  end

  def do_level(nav_items = @nav_items)
    if nav_items.class == Hash
      puts 'obj is hash'

      nav_items.each_pair do |key, val|
        if nav_items[key].class == Array
          do_level(nav_items[key])
        else
          do_level(nav_items[key])
        end
      end
    elsif nav_items.class == Array
      puts 'obj is array'

      nav_items.each do |item|
        if item.class == Array
          do_level(item)
        else
          conv(item)
        end
      end
    else
      puts 'what‽'
    end
  end
end


AdocWiki.new('./nav.yml').do_level

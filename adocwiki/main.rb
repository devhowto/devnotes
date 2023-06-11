require 'yaml'
require 'fileutils'
require 'asciidoctor'
require 'erb'

h = YAML.load_file('./nav.yml')


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

def do_level(obj)
  if obj.class == Hash
    puts 'obj is hash'

    obj.each_pair do |key, val|
      if obj[key].class == Array
        do_level(obj[key])
      else
        do_level(obj[key])
      end
    end
  elsif obj.class == Array
    puts 'obj is array'

    obj.each do |item|
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

do_level(h)

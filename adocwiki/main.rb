require 'yaml'
require 'fileutils'
require 'asciidoctor'

h = YAML.load_file('./nav.yml')

def conv(file)
  puts "file: #{file}"
  arr = file.split('/')

  # Drop last element, the file with the .adoc extension.
  dirs = arr[0 .. -2]

  # Prepend ‘build’ to the path.
  dirs.insert(0, 'build')

  outname = arr[-1]

  FileUtils.mkpath(dirs.join('/'))

  # puts "dirs: #{dirs}"
  # puts "outname: #{outname}"
  # puts "outfile: #{dirs.join('/')}/#{outname}"

  adoc = Asciidoctor.convert_file(
    file,
    to_file: "#{dirs.join('/')}/#{outname.gsub(/adoc$/, 'html')}",
    backend: :html,
    standalone: false
  )

  p adoc.doctitle
  p adoc.attributes
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

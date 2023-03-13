# rubocop:disable Lint/Debugger

def contains_v1(str, sub)
  sub.chars.each_index do |i|
    return true if str[i...sub.size] == sub
  end
end

def contains_v2(str, sub)
  str.include?(sub)
end

str = 'yea im so cool'

p contains_v1('yea im so cool', 'yea')
p contains_v2('yea im so cool', 'yea')
p 'yeah im so cool'.index('yea')

p str['yea']       # => 'yea'
p str.match 'yea'  # => #<MatchData "yea">
p str.match? 'yea' # => true

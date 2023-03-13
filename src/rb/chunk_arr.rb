# rubocop:disable Lint/Debugger

#
# Chat on Ruby:
#
# https://discord.com/channels/518658712081268738/650031651845308419/1083908468467044432
#

#
# Can you chunk/slice an array into sub-arrays, but drop the separator?
#
# That is, accomplish the following without needing the map? This isn't
# a big deal, obviously, but this seems to me like it might already
# exist.
#

p [2, 0, 2, 3, '-', 0, 3, '-', 1, 0].chunk do |elem|
  elem == '-' ? :_separator : true
end.map(&:last)
# => [[2, 0, 2, 3], [0, 3], [1, 0]]

#
# Something similar to String's split:
#
p '2023-03-10'.split('-')
# => ["2023", "03", "10"]

p [2, 0, 2, 3, '-', 0, 3, '-', 1, 0].chunk_while { _1 != '-' }.to_a
# => [[2, 0, 2, 3, "-"], [0, 3, "-"], [1, 0]]

p [2, 0, 2, 3, '-', 0, 3, '-', 1, 0]
  .chunk { _1 != '-' || :_separator }
  .map(&:last)
  .display
# => [[2, 0, 2, 3], [0, 3], [1, 0]]=> nil

#
# If the possible number of bits in the integer is less than the length
# of the string we can shorten it further. If the array is always single
# digit and single chars, the bit at position 4 is never set on ints, so
# we can say the following (edited)
#

p [2, 0, 2, 3, '-', 0, 3, '-', 1, 0]
  .chunk { _1[4] }
  .map(&:last)
# => [[2, 0, 2, 3], [0, 3], [1, 0]]

p [2, 0, 2, 3, '-', 0, 3, '-', 1, 0].each_with_object([[]]) do |i, a|
  i == '-' ? a << [] : a.last << i
end

p 'abcde'.slice(..-3)
# => "abc"

p [2, 0, 2, 3, '-', 0, 3, '-', 1, 0].chunk { _1[4] }.map(&:pop)
# => [[2, 0, 2, 3], [0, 3], [1, 0]]


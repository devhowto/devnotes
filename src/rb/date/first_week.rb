#
# Gets the dates of the first week of the given month in the given year.
#

#
# Parameters:
#
# • month, e.g: 1, 12
# • year, e.g.: 2023, 1984
#

require 'date'

unless [*'1'..'12'].include?(ARGV.first)
  fail 'Month must be an integer between 1 and 12.'
end

unless ARGV[1]
  fail "Provided year ‘#{ARGV[2]}’ is not valid."
end

month, year = ARGV.map(&:to_i)

date = Date.new(year, month, 1)
dates = []

while date.wday < 6
  dates << date
  date = date.next_month
end

dates.each do |d|
  p d
end

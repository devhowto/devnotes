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

# unless [*'1'..'12'].include?(ARGV.first)
#   fail 'Month must be an integer between 1 and 12.'
# end
#
# unless ARGV[1]
#   fail "Provided year ‘#{ARGV[2]}’ is not valid."
# end

class DateUtils
  attr_reader :year, :month, :day, :date

  def initialize(year, month, day)
    @year = year
    @month = month
    @day = day

    @date = Date.new(year, month, day)
  end

  ##
  # Returns the dates for the given week.
  #
  # @param which [Symbol] A week identifier like `:first`, `:second`,
  #   `:third`, `:fourth`, and `:last`.
  # @return [Array<Date>] All dates for that week.
  #
  def week(which)
    return first_week if which == :first
  end

  private

  def first_week
    dates = []
    date = Date.new(year, month, 1)

    dates << date

    6.times do
      date = date.next_day

      break if dates.last.wday == 6

      dates << date
    end

    dates
  end
end
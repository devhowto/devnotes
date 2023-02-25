require 'date'

##
# A utility class for dates.
#
class DateUtils
  attr_reader :year, :month, :day, :date

  ##
  # @param year [Integer]
  # @param month [Integer] A month number between 1 and 12 (inclusive).
  # @param day [Integer] A day of month. Optional. Defaults to 1.
  #
  def initialize(year, month, day = 1)
    @year = year
    @month = month
    @day = day

    @date = Date.new(year, month, day)
  end

  ##
  # Returns the dates for the given week.
  #
  # @param which {:first | :last }
  # @return [Array<Date>] All dates for that week.
  #
  def week(which)
    return first_week if which == :first
    return last_week if which == :last
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

  def last_week
    dates = []

    #
    # -1 makes it the last day of the month.
    #
    date = Date.new(year, month, -1)

    #
    # `date` could potentially be `wday == 6`.
    #
    dates.prepend(date)

    #
    # -2 is the last but one (penultimate) day of the month.
    #
    6.times do
      date = date.prev_day

      break if date.wday == 6

      dates.prepend(date)
    end

    dates
  end
end

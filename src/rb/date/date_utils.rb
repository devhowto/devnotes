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
    return second_week if which == :second
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

  ##
  # Returns an array of the dates on the second week of the month.
  #
  # @return [Array<Date>]
  #
  def second_week
    dates = []

    date = Date.new(year, month, day)

    in_second_week = false

    ##
    # Walk the dates until we reach the second week.
    #
    until in_second_week
      in_second_week = true if date.wday == 6
      date = date.next_day
    end

    ##
    # Every week that is not the first or the last week of the month is
    # guaranteed to contain seven days. Let's fill `dates` with the next
    # seven dates.
    #
    while dates.size < 7
      dates << date
      date = date.next_day
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

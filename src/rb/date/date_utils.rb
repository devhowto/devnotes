require 'date'

#
# $ cal -y 2024
#                                2024
#
#        January               February                 March
# Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
#     1  2  3  4  5  6                1  2  3                   1  2
#  7  8  9 10 11 12 13    4  5  6  7  8  9 10    3  4  5  6  7  8  9
# 14 15 16 17 18 19 20   11 12 13 14 15 16 17   10 11 12 13 14 15 16
# 21 22 23 24 25 26 27   18 19 20 21 22 23 24   17 18 19 20 21 22 23
# 28 29 30 31            25 26 27 28 29         24 25 26 27 28 29 30
#                                               31
#         April                   May                   June
# Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
#     1  2  3  4  5  6             1  2  3  4                      1
#  7  8  9 10 11 12 13    5  6  7  8  9 10 11    2  3  4  5  6  7  8
# 14 15 16 17 18 19 20   12 13 14 15 16 17 18    9 10 11 12 13 14 15
# 21 22 23 24 25 26 27   19 20 21 22 23 24 25   16 17 18 19 20 21 22
# 28 29 30               26 27 28 29 30 31      23 24 25 26 27 28 29
#                                               30
#         July                  August                September
# Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
#     1  2  3  4  5  6                1  2  3    1  2  3  4  5  6  7
#  7  8  9 10 11 12 13    4  5  6  7  8  9 10    8  9 10 11 12 13 14
# 14 15 16 17 18 19 20   11 12 13 14 15 16 17   15 16 17 18 19 20 21
# 21 22 23 24 25 26 27   18 19 20 21 22 23 24   22 23 24 25 26 27 28
# 28 29 30 31            25 26 27 28 29 30 31   29 30
#
#        October               November               December
# Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa   Su Mo Tu We Th Fr Sa
#        1  2  3  4  5                   1  2    1  2  3  4  5  6  7
#  6  7  8  9 10 11 12    3  4  5  6  7  8  9    8  9 10 11 12 13 14
# 13 14 15 16 17 18 19   10 11 12 13 14 15 16   15 16 17 18 19 20 21
# 20 21 22 23 24 25 26   17 18 19 20 21 22 23   22 23 24 25 26 27 28
# 27 28 29 30 31         24 25 26 27 28 29 30   29 30 31
#
# Looking at the calendar for the year 2024 (which is a leap year, by
# the way and thus February has 29 days, not 28), we can notice a few
# things:
#
# • The first week of any given month can have from 1 to 7 days.
# • The last week of any given month can also have from 1 to 7 days.
# • There is never a first or last week of any given month with zero
#   days, or more than seven r days 🤣.
# • There are always between four and six weeks in any given month.
#   Five weeks is more common. Sometimes a month spans six weeks.
#   In more rare cases, if February is not a leap year (therefore it has
#   28 days), and the first day of the month falls on Sunday, then the
#   month will span for four weeks. 28 = 7 * 4.
#
#     $ cal 2 2015
#         February 2015
#     Su Mo Tu We Th Fr Sa
#      1  2  3  4  5  6  7
#      8  9 10 11 12 13 14
#     15 16 17 18 19 20 21
#     22 23 24 25 26 27 28
#
# Those tidbits are useful to keep in mind as they can provide ideas on
# how to implement certain date-related utilities.
#

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

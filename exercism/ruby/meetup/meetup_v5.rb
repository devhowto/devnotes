#
# Solution by Lapizistik on Ruby Discord server.
#
# https://discord.com/channels/518658712081268738/650031651845308419/1080239306356035705
#

require 'date'

class Meetup
  VERSION = 5

  CALENDAR_SCHEDULES = {
    first: 1,
    second: 8,
    third: 15,
    fourth: 22,
    teenth: 13,
  }.freeze

  DAYS_OF_WEEK = %i[
    sunday monday tuesday wednesday thursday friday saturday
  ].map.with_index { |d, i| [d, i] }.to_h

  def initialize(month, year)
    @month = month
    @year = year

    ##
    # The last schedule is month-specific.
    #
    days_in_month = (Date.new(year, month).next_month - 1).day
    @last_schedule = days_in_month - 6
  end

  def day(weekday, schedule)
    wday = DAYS_OF_WEEK[weekday]
    d = Date.new(@year, @month, schedule_start(schedule))

    ##
    # Jump forward one week if we would go back.
    #
    wday += 7 if wday < d.wday
    d + (wday - d.wday)
  end

  def schedule_start(schedule)
    return @last_schedule if schedule == :last

    CALENDAR_SCHEDULES[schedule] or
      raise "Unknown schedule: “#{schedule.inspect}”"
  end
end

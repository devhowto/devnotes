class BirdCount
  ##
  # A “busy day” is one in which five or more birds have
  # visited the garden.
  #
  BUSY_MIN_COUNT = 5

  def self.last_week
    [0, 2, 5, 3, 7, 8, 4]
  end

  def initialize(birds_per_day)
    @per_day = birds_per_day
  end

  def yesterday
    @per_day[-2]
  end

  def total
    @per_day.sum
  end

  def busy_days
    @per_day.select { |count| count >= BUSY_MIN_COUNT }.size
  end

  def day_without_birds?
    @per_day.any?(&:zero?)
  end
end

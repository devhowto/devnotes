Microwave = Class.new do
  def initialize(seconds)
    @seconds = seconds
  end

  def timer
    secs = @seconds.to_s

    if secs.length <= 2
      hours, seconds = @seconds.divmod(60)
      return pad(hours) << ':' << pad(seconds)
    else
      hours = secs[0...1]
      seconds = secs[1..-1]

      return pad(hours) << ':' << pad(seconds)
    end
  end

  private

  ##
  # Pad n with 0 to make sure it is always a two digit string.
  #
  # Simply return the number as string if no need to pad.
  #
  # @param n [Integer|String]
  # @return [String]
  #
  def pad(n)
    s = n.to_s

    return '0' << s if s.length < 2

    return s
  end
end

class Lasagna
  EXPECTED_MINUTES_IN_OVEN = 40

  ##
  # Given how long it has been in the oven, calculate the expected
  # remaining time.
  #
  # @param actual_minutes_in_oven [Integer]
  # @return [Integer]
  #
  def remaining_minutes_in_oven(actual_minutes_in_oven)
    EXPECTED_MINUTES_IN_OVEN - actual_minutes_in_oven
  end

  ##
  # Given the number of layers, calculate the expected amount of
  # time time to prepare the lasagna.
  #
  # @param layers [Integer]
  # @return [Integer]
  #
  def preparation_time_in_minutes(layers)
    layers * 2
  end

  ##
  # Computes the total time to prepare the lasagna.
  #
  # The total time involves preparing the layers + the time it takes
  # in the oven.
  #
  # Note we reuse #preparation_time_in_minutes so if the time it
  # takes to prepare each layer changes then we only need to change
  # that method (#preparation_time_in_minutes) and this method
  # (#total_time_in_minutes) will still work without modification.
  #
  def total_time_in_minutes(number_of_layers:, actual_minutes_in_oven:)
    preparation_time_in_minutes(number_of_layers) + actual_minutes_in_oven
  end
end

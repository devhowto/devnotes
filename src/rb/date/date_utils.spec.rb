require 'rspec'
require_relative 'date_utils'

describe DateUtils do
  context 'first week' do
    #
    # $ cal 1 2023
    #     January 2023
    # Su Mo Tu We Th Fr Sa
    #  1  2  3  4  5  6  7
    #  8  9 10 11 12 13 14
    # 15 16 17 18 19 20 21
    # 22 23 24 25 26 27 28
    # 29 30 31
    #

    it 'should handle January 2023' do
      expected = [
        Date.new(2023, 1, 1), # Sun
        Date.new(2023, 1, 2), # Mon
        Date.new(2023, 1, 3), # Tue
        Date.new(2023, 1, 4), # Wed
        Date.new(2023, 1, 5), # Thu
        Date.new(2023, 1, 6), # Fri
        Date.new(2023, 1, 7), # Sat
      ]

      expect(DateUtils.new(2023, 1, 1).week(:first)).to eq(expected)
    end

    it 'can handle April 2023' do
      #
      # $ cal 4 2023
      #      April 2023
      # Su Mo Tu We Th Fr Sa
      #                    1
      #  2  3  4  5  6  7  8
      #  9 10 11 12 13 14 15
      # 16 17 18 19 20 21 22
      # 23 24 25 26 27 28 29
      # 30
      #

      expected = [
        Date.new(2023, 4, 1),
      ]

      expect(DateUtils.new(2023, 4, 1).week(:first)).to eq(expected)
    end
  end

  it 'can handle November 1984' do
    #
    # $ cal 11 1984
    #     November 1984
    # Su Mo Tu We Th Fr Sa
    #              1  2  3
    #  4  5  6  7  8  9 10
    # 11 12 13 14 15 16 17
    # 18 19 20 21 22 23 24
    # 25 26 27 28 29 30
    #

    expected = [
      Date.new(1984, 11, 1), # Thu
      Date.new(1984, 11, 2), # Fri
      Date.new(1984, 11, 3), # Sat
    ]

    expect(DateUtils.new(1984, 11, 1).week(:first)).to eq(expected)
  end
end

require 'rspec/autorun'

#
# Stubbing class method.
#

class SumFeature
  class << self
    def available?
      false
    end
  end
end

class BasicCalculator
  ##
  # This method has a direct dependency on SumFeature instead of
  # that dependency being injected and made explicit.
  #
  def sum(a, b)
    return 'sum not available' unless SumFeature.available?

    a + b
  end
end

describe BasicCalculator do
  describe '#sum' do
    it 'should return info about sum not available' do
      basic_calc = BasicCalculator.new

      expect(basic_calc.sum(-1, 1)).to eq('sum not available')
    end

    it 'should return correct sum when feature is available' do
      basic_calc = BasicCalculator.new

      allow(SumFeature).to receive(:available?).and_return(true)

      expect(basic_calc.sum(-1, 1)).to eq(0)
    end
  end
end

#
# Remember that in ruby a class is an instance. The class below is
# the same as:
#
#   SumFeature = Class.new do
#     # ...
#   end
#
# Because the method `available?` is defined on the class itself,
# the `allow()` sbut works. (It will be different with if it has a
# instance method.
#

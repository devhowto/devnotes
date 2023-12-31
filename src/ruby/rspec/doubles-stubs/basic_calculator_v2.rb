require 'rspec/autorun'

#
# Stubbing instance method. Requires a double and the double must
# be passed as argument.
#

class BasicCalculator
  def sum(a, b, user)
    return 'sum not available' unless user.math_available?

    a + b
  end
end

describe BasicCalculator do
  describe '#sum' do
    it 'should return info about sum not available' do
      basic_calc = BasicCalculator.new

      user = double('User')
      allow(user).to receive(:math_available?).and_return(false)

      expect(basic_calc.sum(-1, 1, user)).to eq('sum not available')
    end

    it 'should return correct sum when feature is available' do
      basic_calc = BasicCalculator.new

      user = double('User')

      allow(user).to receive(:math_available?).and_return(true)

      expect(basic_calc.sum(-1, 1, user)).to eq(0)
    end
  end
end

#
# For instance method, we need test doubles followed by stubs to
# fake some implementation we need for a giving testing scenario.
#
# The double must then be a paramter or some how injected into the
# code that needs it.
#

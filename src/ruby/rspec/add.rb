require 'rspec/autorun'

class Calc
  class << self
    def add(x, y)
      x + y
    end
  end
end

describe 'Calc.add' do
  it 'should add two numbers' do
    expect(Calc.add(-1, 1)).to eq(0)
  end
end

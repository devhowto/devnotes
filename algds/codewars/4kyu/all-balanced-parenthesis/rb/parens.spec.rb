require 'rspec'
require_relative './parens_v1'

describe 'parens(n)' do
  context 'when the input is 0' do
    it 'should return an array with 1 empty string' do
      expect(parens(0)).to eq([''])
    end
  end

  context 'when the input is 1' do
    it 'should return 1 set of balanced parenthesis' do
      expect(parens(1)).to eq(['()'])
    end
  end

  context 'when the input is 2' do
    it 'should return 2 set of balanced parenthesis' do
      expect(parens(2)).to eq(['(())', '()()'])
    end
  end

  context 'when the input is 3' do
    it 'should return 3 set of balanced parenthesis' do
      expect(
        parens(3)
      ).to eq(['((()))', '(()())', '(())()', '()(())', '()()()'])
    end
  end
end

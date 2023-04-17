

class Num
  class << self
    def fact(n)
      (1..n).inject(1) { |a, n| a * n }
    end
  end
end

p Num.fact(6)

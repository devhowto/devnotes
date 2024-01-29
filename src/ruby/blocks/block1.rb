class Thing
  def run(x, y)
    yield(x, y) if block_given?
  end
end

p Thing.new.run(2, 5) { |a, b| a + b }

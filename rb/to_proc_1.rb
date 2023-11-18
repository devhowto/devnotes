##
# Monkey-patch `Hash` with our own `#to_proc` implementation.
#
# NOTE: With `ruby -w` a warning is reported about overriding
# original `#to_proc`.
#
class Hash
  ##
  # Causes each element to be incremented by one.
  #
  def to_proc = proc do |key|
    self[key] + 1
  end
end

h = { one: 1, two: 2 }

p [:one, :two].map(&h)
#=> [2, 3]

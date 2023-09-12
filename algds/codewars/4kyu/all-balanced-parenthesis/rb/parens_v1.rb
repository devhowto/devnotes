##
# Generates all possible combinations of `n` balanced parenthesis.
#
# @param n {Integer} The number of balanced parenthesis.
# @return {String}
#
def parens(n)
  res = []

  f = lambda do |s, l, r|
    return if (l > r)

    res << s if (l.zero? && r.zero?)

    f.call("#{s}(", l - 1, r) if l > 0

    f.call("#{s})", l, r - 1) if r > 0
  end

  f.call('', n, n)

  res
end

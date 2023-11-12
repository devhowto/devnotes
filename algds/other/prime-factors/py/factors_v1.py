def factors(x, n, j, ps = []):
  #
  # 1 is a unit. Not a prime, and not a composite.
  #
  if n == 1:
    return 1

  i = j
  while i * i <= n:
    #
    # if n is prime
    #
    if n % i == 0:
      ps.append(i)
      factors(x, int(n / i), i, ps)
      return ps
    else:
      i = i + 1

  #
  # If we got to this point, x is prime.
  #
  if x == n:
    return "%d is prime" % x
  else:
    ps.append(n)
    return ps


print(factors(18, 18, 2, []))
#=> [2, 3, 3]

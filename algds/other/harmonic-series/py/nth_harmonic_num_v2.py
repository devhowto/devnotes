def nth_harmonic_num(n):
  h = 1.0

  for i in range(2, n + 1):
    h += 1 / n

  return h

print(nth_harmonic_num(8))

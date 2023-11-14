def nth_harmonic_num(n):
  if n == 1:
    return 1.0

  return nth_harmonic_num(n - 1) + 1 / n

print(nth_harmonic_num(4))
print(nth_harmonic_num(8))

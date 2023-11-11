from math import sqrt

def factors(x, n, j):
  if n == 1:
    print('1 is a unit')
    return

  i = j
  while i <= int(sqrt(n)):
  # while i * i <= n:
    if n % i == 0:
      print("%d is prime" % i)
      factors(x, int(n / i), i)
      return
    else:
      i = i + 1

  if x == n:
    print("%d, our x, is prime" % x)
  else:
    print("What?", x, n, i)


# factors(1, 1, 2)
factors(18, 18, 2)

#
# tags: max int list array find search
#

##
# Find the two max int values in the list.
#
# ASSUME: The list contains at least two ints.
#
def find_two_max(xs):
  best = xs[0]
  second_best = xs[1]

  for x in xs[2:]:
    if x > best:
      second_best = best
      best = x
    elif x > second_best:
      second_best = x

  return [best, second_best]

print(find_two_max([7, 3]))
#=> [7, 3]

print(find_two_max([7, 3, 5, 9, 2, 1, 11]))
#=> [11, 9]

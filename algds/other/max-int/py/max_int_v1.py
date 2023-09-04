#
# tags: max int list array find search
#

##
# Find the max int value.
#
# ASSUME: The list contains at least one int.
#
def find_max(xs):
  max = xs[0]

  for x in xs[1:]:
    if x > max:
      max = x

  return max


print(find_max([7]))
print(find_max([7, 1, 4]))
print(find_max([7, 1, 4, 9, 11, 1]))

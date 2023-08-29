#
# LeetCode: House Robber :: Medium
#
# tags: dynamic-programming
#

##
# • T.C: O(n)
# • S.C: O(1)
#
def rob(nums):
  max1, max2 = 0, 0

  for n in nums:
    tmp = max(max1, n + max2)
    max2 = max1
    max1 = tmp

  return max1



print(rob([]))
#=> 0

print(rob([7]))
#=> 7

print(rob([3, 1]))
#=> 3

print(rob([1, 2, 5]))
#=> 6

print(rob([1, 2, 3, 1]))
#=> 4

print(rob([2, 7, 9, 3, 1]))
#=> 12

print(rob([7, 7, 8, 7]))
#=> 15

print(rob([3, 5, 1, 9, 9]))
#=> 14

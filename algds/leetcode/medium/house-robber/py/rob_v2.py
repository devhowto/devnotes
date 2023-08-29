#
# LeetCode: House Robber :: Medium
#
# tags: dynamic-programming
#

##
# • T.C: O(n)
# • S.C: O(n)
#
def rob(nums):
  if len(nums) == 0:
    return 0

  if len(nums) == 1:
    return nums[0]

  if len(nums) == 2:
    return max(nums[0], nums[1])

  memo = [nums[0], nums[1]]

  for n in nums[2:]:
    memo.append(max(memo[-1], n + memo[-2]))

  return memo[-1]

#
# Using a list to keep track of the history of maximum values “so far”.
#
# It ends up requiring more conditions and space.
#

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

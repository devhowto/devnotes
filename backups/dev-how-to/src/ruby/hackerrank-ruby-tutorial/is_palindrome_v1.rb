##
# Checks wheter `s` is a palindrome.
#
# This is a recursive solution.
#
# ASSUME: `s` is a string with no uppercase chars.
#
# REFERENCES:
#
# • https://www.dictionary.com/e/palindromic-word/
#
def palind?(s)
  return true if [0, 1].include?(s.size)
  return false if s[0] != s[-1]

  palind?(s[1, s.size - 2])
end

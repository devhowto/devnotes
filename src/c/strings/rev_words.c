#include <stdio.h>
#include <string.h>
#include <criterion/criterion.h>

/**
 * Reverses the words in `s`.
 *
 * A word is any number of characters delimited by a space or
 * the NULL terminator `\0`.
 */
void rev_words(char *s, char *r) {
  short cnt = 0, i, j;

  for (i = 0; s[i] != '\0'; ++i) {
    ++cnt;

    //
    // If the next element does not delimit the end of the current
    // word, simply move on.
    //
    if (s[i + 1] != ' ' && s[i + 1] != '\0')
      continue;

    //
    // We are at the last char of the current word. Reverse it!
    //
    for (j = 0; j < cnt; ++j)
      r[i + 1 - cnt + j] = s[i - j];

    cnt = -1;

    r[i + 1] = s[i + 1];
  }
}

// int main(void) {
//   char str[] = "hey you should know better by now";
//   char res[56] = { '\0' };
//
//   rev_words(str, res);
//
//   printf("%s\n", res);
//   return 0;
// }

void tester(char *str, char *rev);

Test(rev_words, should_reverse_all_words) {
  tester("Hello", "olleH");
  tester("may the force", "yam eht ecrof");
}

void tester(char *str, char *expected) {
  char res[96] = { '\0' };

  rev_words(str, res);

  if (strcmp(res, expected)) {
    cr_assert_fail(
      "String: %s\nReversed: %s\nExpeted: %s",
              str,          res,     expected
    );
  }

  cr_assert(1);
}

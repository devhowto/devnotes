#include <stdio.h>

void rev_words(char *s, char* r) {
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
    // reverse the current word.
    //

    for (j = 0; j < cnt; ++j)
      r[i + 1 - cnt + j] = s[i - j];

  }
}

// h e y   y o u
//     •

// _ _ _

int main(void) {
  char str[] = "hey you";
  char res[8] = { '\0' };

  rev_words(str, res);

  printf("%s\n", res);
  return 0;
}


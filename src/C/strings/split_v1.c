#include <stdio.h>

#define MAX_WORDS 13
#define MAX_WORD_LEN 13

void split(const char* s) {
  char words[MAX_WORDS][MAX_WORD_LEN];

  //
  // Word index and char index.
  //
  short word_idx, char_idx;

  word_idx = char_idx = 0;
  char c;

  while ((c = *s++) != '\0') {
    if (c == ' ') {
      words[word_idx][char_idx + 1] = '\0';
      word_idx += 1;
      char_idx = 0;
    }
    else {
      words[word_idx][char_idx++] = c;
    }
  }

  words[word_idx][char_idx + 1] = '\0';

  for (word_idx = 0; word_idx < 3; ++word_idx)
    printf("%s\n", *(words + word_idx));
}

int main(void) {
  split("May the force be with you.");

  return 0;
}

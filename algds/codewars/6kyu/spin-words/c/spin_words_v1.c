//
// tags: codewars haskell algorithm string
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void spin_words(const char* s) {
  char* word = strdup(s); // malloc(13 * (sizeof (char *)));
  word = strdup(s);

  // word = strtok((char *) s, " ");
  printf("%s\n", word);
}

int main(void) {
  spin_words("hello my wonderful world");

  return 0;
}

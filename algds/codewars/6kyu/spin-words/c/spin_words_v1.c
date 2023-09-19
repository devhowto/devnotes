//
// tags: codewars haskell algorithm string
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void spin_words(const char* s) {
  char* cpy = strdup(s);
  char* tok = malloc(64);

  tok = strtok(cpy, " ");

  while (
  printf("%s\n", tok);
}

int main(void) {
  spin_words("hello my wonderful world");

  return 0;
}

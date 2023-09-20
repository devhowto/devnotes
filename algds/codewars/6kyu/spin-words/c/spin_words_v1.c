//
// tags: codewars haskell algorithm string
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Split a string. Save split parts in r and returns length
 * of resulting array.
 */
char** str_split(const char* str, char* delim, short* len) {
  *len = 0;
  char* cpy = strdup(str);
  char* tok = 0;
  char** res = malloc(sizeof (char) * (strlen(cpy) / 2));

  tok = strtok(cpy, delim);

  while (tok != NULL) {
    *(res + (*len)++) = strdup(tok);
    tok = strtok(NULL, delim);
  }

  return res;
}

void spin_words(const char* s) {
  // Copy so we own cpy mem.
  char* cpy = strdup(s);
}

int main(void) {
  short len;

  char** res = str_split("Use the force, luke.", " ", &len);

  printf("%d\n", len);

  for (short i = 0; i < len; ++i)
    printf("w: %s\n", *(res + i));

  return 0;
}

#include <stdio.h>

/**
 * Reverse a string in-place.
 */
void rev(char *str) {
  char *end = str;
  char chr;

  while (*end++)
    ;

  end -= 2;

  while (str < end) {
    chr = *str;
    *str++ = *end;
    *end-- = chr;
  }
}

int main(void) {
  char s[] = "abc";

  rev(s);

  printf("%s\n", s);

  return 0;
}

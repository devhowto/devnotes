#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// void strdup(const char* src, char* dst) {
//   while ((*dst++ = *src++))
//     ;
// }

void ptok(char* s) {
  char* tok = strtok(s, " ");
  while (tok != NULL) {
    printf("tok: %s\n", tok);
    tok = strtok(NULL, " ");
  }
}

int main(void) {
  // Causes segfault
  // char* s = "may the source";

  // Works
  char s[] = "may the source be with you";

  ptok(s);

  return 0;
}

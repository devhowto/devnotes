#include <stdio.h>
#include "encrypt.h"

#define MAX_LEN 64

/**
 * Provide word or sentence parameter as the first argument.
 */
int main(void) {
  char msg[64];

  char w[] = "hello";
  encrypt(w);
  printf("hello → “%s”\n", w);

  // while (fgets(msg, 64, stdin) != NULL) {
  //   printf("%s\n", msg);
  //   encrypt(msg);
  //   printf("%s", msg);
  // }

  return 0;
}

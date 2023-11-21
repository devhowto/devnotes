#include <stdio.h>
#include <stdlib.h>
#include "encrypt.h"

void encrypt(char *msg) {
  while (*msg) {
    *msg = *msg ^ 31;
    ++msg;
  }
}

/**
 * Privide word or sentence parameter as the first argument.
 */
int main(int argc, char *argv[]) {
  char s[] = "hello";

  // Encrypt
  encrypt(s);
  printf("%s\n", s);

  // Revert the encryption.
  encrypt(s);
  printf("%s\n", s);

  return 0;
}

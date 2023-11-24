#include <stdio.h>
#include "encrypt.h"

void encrypt(char *msg) {
  while (*msg) {
    printf("%c\n", *msg);
    *msg = *msg ^ 31;
    msg++;
  }
}

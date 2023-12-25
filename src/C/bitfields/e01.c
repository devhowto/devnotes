#include <stdio.h>

#define NUM_BITS 8

void printbits(unsigned char v) {
  printf("%d\n", '0');
  short i;

  for(i = NUM_BITS - 1; i >= 0; --i)
    putchar('0' + ((v >> i) & 1));
}

int main(void) {
  printbits(0x2);

  return 0;
}

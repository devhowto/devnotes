/**
 * Factorial witl tail recursion.
 */

#include <stdio.h>
#include "factail.h"

int fact(int n, int acc) {
  if (n < 0) return 0;
  else if (n == 0 || n == 1) return acc;
  else return fact(n - 1, n * acc);
}

int main(void) {
  printf("%d\n", fact(12, 1));
  return 0;
}

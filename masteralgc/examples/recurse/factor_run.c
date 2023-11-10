#include <stdio.h>
#include "factor.h"

int main(void) {
  int n;

  printf("\nFactoring 18:\n");
  factor(18, 18, 2);

  printf("\nFactoring 19:\n");
  factor(19, 19, 2);

  printf("\nFactoring 20:\n");
  factor(20, 20, 2);

  // for (n = 0; n <= 10; ++n) {
  //   printf("Factoring %d:\n", n);
  //   factor(n, n, 2);
  // }

  return 0;
}

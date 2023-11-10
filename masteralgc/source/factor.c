#include <stdio.h>
#include <math.h>
#include "factor.h"

void factor(int x, int n, int j) {
  int i;

  //
  // 1 is neither prime nor composite.
  //
  if (n == 1) {
    printf("1 is a unit.\n");
    return;
  }

  //
  // Determine the prime factors of n.
  //
  i = j;

  while (i <= (int)(sqrt((double)n))) {
    if (n % i == 0) {
      //
      // We have found a factor of n. Print it and
      // factor n / i.
      //
      printf("%d\n", i);
      factor(x, (int)(n / i), i);
      return;
    } else {
      ++i;
    }
  }

  //
  // If this point is reached, n is prime.
  //
  if (n == x)
    printf("%d is prime.\n", x);
  else
    printf("%d is not prime.\n", n);

  return;
}

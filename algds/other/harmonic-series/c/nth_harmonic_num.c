#include <stdio.h>

/**
 * Compute the nth harmonic number.
 *
 * ASSUME: n >= 1.
 */
double nth_harmonic_num(short n) {
  if (n == 1) return 1.0;
  return nth_harmonic_num(n - 1) + 1.0 / n;
}

int main(void) {
  printf("%.4f\n", nth_harmonic_num(4));
  printf("%.4f\n", nth_harmonic_num(8));
  printf("%.4f\n", nth_harmonic_num(9));

  return 0;
}

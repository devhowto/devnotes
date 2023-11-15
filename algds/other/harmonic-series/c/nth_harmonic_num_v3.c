#include <stdio.h>

/**
 * Computes the nth harmonic term using tail recursion.
 *
 * ASSUME: n >= 1
 */
double nth_harmonic_term(int n, float h) {
  if (n == 1) return h + 1;
  return nth_harmonic_term(n - 1, h + 1.0 / n);
}

int main(void) {

  // printf("%.4f\n", nth_harmonic_term(4, 0));
  // printf("%.4f\n", nth_harmonic_term(8, 0));
  // printf("%.4f\n", nth_harmonic_term(9, 0));
  printf("%.4f\n", nth_harmonic_term(2000, 0));

  return 0;
}

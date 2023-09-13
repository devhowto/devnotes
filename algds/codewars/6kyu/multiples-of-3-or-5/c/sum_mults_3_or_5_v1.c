//
// tags: javascript codewars 6kyu math algorithm multiple
//

#include <stdio.h>

/**
 * Checks whether `y` is a multiple of `x`.
 */
int is_mult_of(int x, int y) {
  return y % x == 0;
}

int sum_mults_3_or_5(int n) {
  int total = 0;

  for (int i = 1; i < n; ++i)
    if (is_mult_of(5, i) || is_mult_of(3, i))
      total += i;

  return total;
}

int main(void) {

  printf("%d\n", sum_mults_3_or_5(10));

  return 0;
}

#include <bits/stdc++.h>

using namespace std;

/**
 * T.C: O(n) because only one single loop is involved.
 *
 * This is a dynamic programming approach using the
 * Kandane's algorithm.
 */
int find_max_sum(const int *xs, size_t len) {
  int best = 0,
      sum = 0;

  for (size_t k = 0; k < len; ++k) {
    sum = max(xs[k], sum + xs[k]);
    best = max(sum, best);
  }

  return best;
}

int main(void) {
  // int xs[] = { 1, 2 };
  int xs[] = {-1, 2, 4, -3, 5, 2, -5, 2};

  int max_sum = find_max_sum(xs, 8);

  printf("%d\n", max_sum);
  //=> 10

  return 0;
}

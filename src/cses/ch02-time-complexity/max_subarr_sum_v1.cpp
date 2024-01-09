#include <bits/stdc++.h>

using namespace std;

/**
 * T.C: O(n³) because of three nested loops.
 */
int find_max_sum(const int *xs, size_t len) {
  int best = 0;

  for (size_t i = 0; i < len; ++i) {
    for (size_t j = i; j < len; ++j) {
      int sum = 0;

      for (size_t k = i; k <= j; ++k)
        sum += *(xs + k);

      best = max(best, sum);
    }
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

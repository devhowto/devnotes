#include <bits/stdc++.h>

using namespace std;

/**
 * T.C: O(n²) because of two nested loops.
 */
int find_max_sum(const int *xs, size_t len) {
  int best = 0;

  for (size_t i = 0; i < len; ++i) {
    int sum = 0;

    for (size_t j = i; j < len; ++j) {
      sum += *(xs + j);
      best = max(sum, best);
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

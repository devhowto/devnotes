//
// tags: c math number-theory sum digital-root recursion
//

#include <stdio.h>

/**
 * • T.C: O(1). Just three arithmetic operations. No loops!
 * • S.C: O(1).
 */
int digital_root(int num) {
  return (num - 1) % 9 + 1;
}

int main(void) {
  printf("%d\n", digital_root(16));
  printf("%d\n", digital_root(942));
  printf("%d\n", digital_root(132189));
  printf("%d\n", digital_root(493193)); // 29,

  return 0;
}

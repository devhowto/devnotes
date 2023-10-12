#include <stdio.h>

int sum_digits(int num, int acc) {
  if (num <= 9) return num + acc;

  // The remainder is the last digit.
  int r = num % 10;

  // The division without the remainder. This is an integer
  // containing all digits except the last one.
  int d = num / 10;

  return sum_digits(d, acc + r);
}

int digital_root(int num) {
  if (num <= 9) return num;

  int total = sum_digits(num, 0);

  return digital_root(total);
}

int main(void) {
  printf("%d\n", digital_root(16));
  printf("%d\n", digital_root(942));
  printf("%d\n", digital_root(132189));
  printf("%d\n", digital_root(493193)); // 29,

  return 0;
}

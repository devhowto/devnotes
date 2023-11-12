//
// tags: c algorithm recursion prime factor division
//

#include <stdio.h>
#include <stdlib.h>

#define MAX_LEN 8

int factors(int n, int c, int *ps, int i) {
  if (n <= 1) return i;
  else if (n % c == 0) {
    *(ps + i) = c;
    return factors(n / c, c, ps, i + 1);
  } else {
    return factors(n, c + 1, ps, i);
  }
}

int main(void) {
  int *ps = malloc(sizeof(int) * MAX_LEN);
  int len;

  len = factors(24, 2, ps, 0);
  printf("\nFound %d prime factors for 24:\n", len);
  for (int i = 0; i < len; ++i)
    printf("• %d\n", *(ps + i));

  len = factors(101, 2, ps, 0);
  printf("\nFound %d prime factors for 101:\n", len);
  for (int i = 0; i < len; ++i)
    printf("• %d\n", *(ps + i));

  len = factors(315, 2, ps, 0);
  printf("\nFound %d prime factors for 315:\n", len);
  for (int i = 0; i < len; ++i)
    printf("• %d\n", *(ps + i));

  free(ps);

  return 0;
}

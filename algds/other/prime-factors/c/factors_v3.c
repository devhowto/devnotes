#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int factor(int x, int n, int j, int *ps, int idx) {
  int i;

  if (n == 1) {
    printf("1 is a unit.\n");
    return ++idx;
  }

  i = j;

  while (i * i <= n) {
    if (n % i == 0) {
      *(ps + idx) = i;
      factor(x, (int)(n / i), i, ps, ++idx);
      return ++idx;
    } else {
      ++i;
    }
  }

  if (n == x)
    printf("%d is prime.\n", x);
  else
    *(ps + idx) = n;

  return ++idx;
}

int main(void) {
  int *ps = malloc(sizeof(int) * 4);
  int i, len;

  len = factor(18, 18, 2, ps, 0);

  for (i = 0; i <= len; ++i)
    printf("%d\n", *(ps + i));

  free(ps);
}

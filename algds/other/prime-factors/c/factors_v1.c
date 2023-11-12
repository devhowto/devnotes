#include <stdio.h>
#include <stdlib.h>

#define MAX_LEN 8

int factors(int n, int *ps) {
  int len = 0,
      d;

  for (d = 2; n > 1; ++d)
    while (n % d == 0) {
      *(ps + len++) = d;
      n = n / d;
    }

  return len;
}

int main(void) {
  int *ps = malloc(sizeof(int) * MAX_LEN);
  int len;

  len = factors(24, ps);
  printf("\nFound %d prime factors for 24:\n", len);
  for (int i = 0; i < len; ++i)
    printf("• %d\n", *(ps + i));

  len = factors(101, ps);
  printf("\nFound %d prime factors for 101:\n", len);
  for (int i = 0; i < len; ++i)
    printf("• %d\n", *(ps + i));

  len = factors(315, ps);
  printf("\nFound %d prime factors for 315:\n", len);
  for (int i = 0; i < len; ++i)
    printf("• %d\n", *(ps + i));


  free(ps);

  return 0;
}

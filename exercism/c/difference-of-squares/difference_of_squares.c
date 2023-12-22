#include <stdlib.h>
#include "difference_of_squares.h"

unsigned int square_of_sum(unsigned int num) {
  unsigned int r = 0;
  size_t i;

  for (i = 1; i <= num; ++i)
    r += i;

  return r * r;
}

unsigned int sum_of_squares(unsigned int num) {
  unsigned int r = 0;
  size_t i;

  for (i = 1; i <= num; ++i)
    r += i * i;

  return r;
}

unsigned int difference_of_squares(unsigned int num) {
  return square_of_sum(num) - sum_of_squares(num);
}

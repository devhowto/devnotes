#include <stdio.h>
#include <stdarg.h>

enum drink {
  MUDSLIDE,
  FUZZY_NAVEL,
  MONKEY_GLAND,
  ZOMBIE,
};

double price(enum drink d) {
  switch(d) {
    case MUDSLIDE:
      return 6.79;
    case FUZZY_NAVEL:
      return 5.31;
    case MONKEY_GLAND:
      return 4.82;
    case ZOMBIE:
      return 5.89;
  }

  return 0;
}

double total(int len, ...) {
  va_list ap;
  va_start(ap, len);
  short i;
  double total = 0.0;

  for (i = 0; i < len; ++i)
    total += price(va_arg(ap, enum drink));

  va_end(ap);

  return total;
}

int main(void) {
  printf(
    "Price: %.2f\n",
    total(3, MONKEY_GLAND, MUDSLIDE, FUZZY_NAVEL));

  printf(
    "Price: %.2f\n",
    total(1, ZOMBIE));

  return 0;
}

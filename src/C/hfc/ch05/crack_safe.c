#include <stdio.h>

typedef struct Swag_ {
  const char *description;
  float value;
} Swag;

typedef struct Combination_ {
  Swag *swag;
  const char *sequence;
} Combination;

typedef struct Safe_ {
  Combination numbers;
  const char *make;
} Safe;

int main(void) {
  Swag gold = { "Gold!", 1000000.0 };
  Combination numbers = { &gold, "6052" };
  Safe safe = { numbers, "RAMACON250" };

  printf("%s\n", safe.numbers.swag->description);
  printf("%s\n", (*safe.numbers.swag).description);

  return 0;
}

#include <stdio.h>

/* The compiler sets aside memory for the larges of the data types,
 * in this case, float. */
typedef union Quantiy_ {
  short count;
  float weight;
  float volume;
} Quantity;

typedef struct FruitOrder_ {
  const char *name;
  const char *country;
  Quantity amount;
} FruitOrder;

int main(void) {
  /* Initialize apples with “designated initializers (>= c99). */
  FruitOrder apples = {
    .name = "Apples",
    .country = "England",
    .amount.weight = 4.2
  };

  printf("This order contains %2.2f lbs of %s from %s.\n",
      apples.amount.weight, apples.name, apples.country);

  return 0;
}

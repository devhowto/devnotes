#include <stdio.h>

typedef enum UnitOfMeasure_ {
  COUNT,
  POUNDS,
  PINTS,
} UnitOfMeasure;

typedef union Quantity_ {
  short count;
  float weight;
  float volume;
} Quantity;

typedef struct FruitOrder_ {
  const char *name;
  const char *country;
  Quantity amount;
  UnitOfMeasure units;
} FruitOrder;

void display(FruitOrder order) {
  printf("This order contains ");

  if (order.units == PINTS)
    printf("%2.2f pints of %s.\n", order.amount.volume, order.name);
  else if (order.units == POUNDS)
    printf("%2.2f libs of %s.\n", order.amount.weight, order.name);
  else
    printf("%hd %s.\n", order.amount.count, order.name);
}

int main(void) {
  FruitOrder apples = {
    "apples",
    "England",
    .amount.count = 144,
    .units = COUNT
  };

  FruitOrder strawberries = {
    "straberries",
    "Spain",
    .amount.weight = 17.6,
    .units = POUNDS
  };

  FruitOrder orange_juice = {
    "orange juice",
    "U.S.A",
    .amount.volume = 10.5,
    .units = PINTS
  };

  display(apples);
  display(strawberries);
  display(orange_juice);

  return 0;
}

/*
 * This order contains 144 apples.
 * This order contains 17.60 libs of straberries.
 * This order contains 10.50 pints of orange juice.
 *
 * REMEMBER: Designated initializers is a feature of C >= 99.
 */

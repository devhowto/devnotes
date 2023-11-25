#include <stdio.h>

typedef struct Equipment_ {
  float tank_capacity;
  short tank_psi;
  const char *suit_material;
} Equipment;

typedef struct ScubaDiver_ {
  const char *name;
  Equipment kit;
} ScubaDiver;

void badge(ScubaDiver diver) {
  printf("Name: %s, Tank: %2.2f(%i), Suit: %s\n",
      diver.name,
      diver.kit.tank_capacity,
      diver.kit.tank_psi,
      diver.kit.suit_material);
}

int main(void) {
  ScubaDiver lara_croft = {
    "Lara Croft",
    {
      5.5,
      3500,
      "Neoprene",
    }
  };

  badge(lara_croft);

  return 0;
}

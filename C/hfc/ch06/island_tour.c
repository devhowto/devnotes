#include <stdio.h>

typedef struct Island_ {
  char *name;
  char *opens;
  char *closes;
  struct Island_ *next;
} Island;

void display(Island *start) {
  Island *island = start;

  while (island != NULL) {
    printf("Name: %s, open: %s-%s.\n",
        island->name, island->opens, island->closes);

    island = island->next;
  }
}

int main(void) {
  Island amity = {
    .name = "Amity",
    .opens = "9:00",
    .closes = "17:00",
    NULL,
  };

  Island craggy = {
    .name = "Craggy",
    .opens = "9:00",
    .closes = "17:00",
    NULL,
  };

  Island skull = {
    .name = "Skull",
    .opens = "9:00",
    .closes = "17:00",
    NULL,
  };

  Island nublar = {
    "Nublar",
    "9:00",
    "17:00",
    NULL,
  };

  Island shutter = {
    .name = "Shutter",
    .opens = "9:00",
    .closes = "17:00",
    NULL,
  };

  amity.next = &craggy;
  craggy.next = &skull;
  skull.next = &nublar;
  nublar.next = &shutter;

  display(&amity);

  return 0;
}

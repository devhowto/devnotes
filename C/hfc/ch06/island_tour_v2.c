/*
 * Because we are using strdup(), make sure to compile this code with a
 * C variant that includes strdup() from string.h. For example,
 * -std=c99 will hide strdup(), but -std=gnu99 will include it.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NAME_LEN 80

typedef struct Island_ {
  char *name;
  char *opens;
  char *closes;
  struct Island_ *next;
} Island;

/**
 * TODO: strdup(name) is causing memory leak.
 */
Island *create(char *name) {
  Island *i = malloc(sizeof(Island));

  i->name = strdup(name);
  i->opens = "9:00";
  i->closes = "17:00";
  i->next = NULL;

  return i;
}

void display(Island *start) {
  Island *island = start;

  while (island != NULL) {
    printf("Name: %s, open: %s-%s.\n",
        island->name, island->opens, island->closes);

    island = island->next;
  }
}

int main(void) {
  char name[MAX_NAME_LEN];

  printf("Island name: ");
  fgets(name, MAX_NAME_LEN, stdin);
  name[strcspn(name, "\r\n")] = '\0';
  Island *p_island0 = create(name);

  printf("Island name: ");
  fgets(name, MAX_NAME_LEN, stdin);
  name[strcspn(name, "\r\n")] = '\0';
  Island *p_island1 = create(name);

  display(p_island1);
  display(p_island0);

  free(p_island0);
  free(p_island1);

  return 0;
}

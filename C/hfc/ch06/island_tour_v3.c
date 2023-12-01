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

void release(Island *start) {
  Island *i = start;
  Island *next = NULL;

  for (; i != NULL; i = next) {
    next = i->next;
    free(i->name);
    free(i);
  }
}

char *read_name(char *ptr) {
  printf("Ilsand name: ");

  if (fgets(ptr, MAX_NAME_LEN, stdin) == NULL)
    return NULL;

  ptr[strcspn(ptr, "\r\n")] = '\0';
  return ptr;
}

int main(void) {
  Island *start = NULL;
  Island *i = NULL;
  Island *next = NULL;
  char name[MAX_NAME_LEN];

  for (; read_name(name) != NULL; i = next) {
    next = create(name);

    if (start == NULL)
      start = next;
    if (i != NULL)
      i->next = next;
  }

  printf("\n\n");
  display(start);

  release(start);

  return 0;
}

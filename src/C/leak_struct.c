#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Jedi_ {
  char *name;
  short level;
} Jedi;

Jedi *create(const char *name, short level) {
  Jedi *jedi = malloc(sizeof(Jedi));

  /* Assign a copy of the string, not a reference to
   * the original string. */
  jedi->name = strdup(name);
  jedi->level = level;

  return jedi;
}

int main(void) {
  Jedi *ahsoka = create("Ahsoka Tano", 97);

  printf("%s is level %hd.\n", ahsoka->name, ahsoka->level);

  /* memory leak without this line as if we first free the struct,
   * we lose reference to the dynamically allocated name field. */
  // free(ahsoka->name);
  free(ahsoka);

  return 0;
}

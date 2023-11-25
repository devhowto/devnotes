#include <stdio.h>

struct Jedi {
  short id;
  const char *name;
};

int main(void) {
  struct Jedi yoda = { 1, "Yoda" };
  struct Jedi master = yoda;

  /* Book says the string pointer should copy the pointer,
   * but printing the addresses below point to different
   * memory locations. */
  struct Jedi ahsoka = { 2, "Ahsoka Tano" };

  printf("\nid: %hd, name: %s\n", yoda.id, yoda.name);
  printf("%p\n", (void *)&yoda.name);

  printf("\nid: %hd, name: %s\n", ahsoka.id, ahsoka.name);
  printf("%p\n", (void *)&ahsoka.name);

  printf("\nid: %hd, name: %s\n", master.id, master.name);
  printf("%p\n", (void *)&master.name);

  return 0;
}

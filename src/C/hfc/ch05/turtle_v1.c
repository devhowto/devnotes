#include <stdio.h>

typedef struct Turtle_ {
  const char *name;
  const char *species;
  short age;
} Turtle;

/**
 * Increments the age by 1 (year) and prints a congratulatory message
 * (but there is a problem with the increment thing).
 *
 * NOTE a pointer is not passed. A “pass by copy” param was performed,
 * which means updating turtle inside the function does not reflect
 * the value of turtle fields outside this function.
 */
void happy_birthday(Turtle turtle) {
  turtle.age = turtle.age + 1;

  // Will print age as 100 here, inside the function.
  printf("Happy birthday, %s! You are now %hd years old!\n",
      turtle.name, turtle.age);
}

int main(void) {
  Turtle myrtle = { "Myrtle", "Leatherback Sea Turtle", 99 };

  happy_birthday(myrtle);

  // Age is still 99.
  printf("%s is now %hd years old.\n", myrtle.name, myrtle.age);

  return 0;
}

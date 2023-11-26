#include <stdio.h>

typedef struct Turtle_ {
  const char *name;
  const char *species;
  short age;
} Turtle;

/**
 * Increments the age by 1 (year) and prints a congratulatory message.
 *
 * We now pass a pointer to the turtle so the age increment is made
 * on the original turtle reference.
 *
 * The parentheses are important due to precedence rules.
 */
void happy_birthday(Turtle *turtle) {
  (*turtle).age = (*turtle).age + 1;

  // Will print age as 100 here, inside the function.
  printf("Happy birthday, %s! You are now %hd years old!\n",
      (*turtle).name, (*turtle).age);
}

int main(void) {
  Turtle myrtle = { "Myrtle", "Leatherback Sea Turtle", 99 };

  // Pass the address (pointer) of the turtle.
  happy_birthday(&myrtle);

  // Age is still 99.
  printf("%s is now %hd years old.\n", myrtle.name, myrtle.age);

  return 0;
}

/*
 * Instead of using parenthesis, (*t).f, the syntax t->f is always
 * possible when t is a pointer to the struct.
 */

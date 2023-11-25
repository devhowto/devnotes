#include <stdio.h>

struct Prefs {
  const char* food;
  float exercise_hours;
};

struct Fish {
  const char *name;
  const char *species;
  short teeth;
  short age;
  struct Prefs care;
};

/* Note the nested struct. */
struct Fish snappy = { "Snappy", "Piranha", 69, 4, { "Meat", 7.5 } };

void catalog(struct Fish fish) {
  printf("%s is a %s with %hd teeth. It is %hd years old.\n",
      fish.name, fish.species, fish.teeth, fish.age);
  printf("It likes to eat %s.\n", fish.care.food);
}

void label(struct Fish fish) {
  printf("Name: %s\nSpecies: %s\n%hd years old,\n%hd teeth\n",
      fish.name, fish.species, fish.teeth, fish.age);
}

int main(void) {
  catalog(snappy);
  label(snappy);

  return 0;
}

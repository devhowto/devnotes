#include <stdio.h>

struct Fish {
  const char *name;
  const char *species;
  short teeth;
  short age;
};

/* Define a struct of type Fish and pass the data like if defining
 * an array. Provide the data in the order they are defined in the
 * struct type. */
struct Fish snappy = {"Snappy", "Piranha", 69, 4};

void catalog(struct Fish fish) {
  printf("%s is a %s with %hd teeth. It is %hd years old.\n",
      fish.name, fish.species, fish.teeth, fish.age);
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

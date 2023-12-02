#include <stdio.h>

struct Meal {
  const char *ingredients;
  float weight;
};

struct Exercise {
  const char *description;
  float duration;
};

struct Preferences {
  struct Meal food;
  struct Exercise exercise;
};

struct Fish {
  const char *name;
  const char *species;
  short teeth;
  short age;
  struct Preferences care;
};

/* Note the nested struct. */
struct Fish snappy = {
  "Snappy",
  "Piranha",
  69,
  4,
  {
    { "meat", 0.2 },
    { "swim in the jacuzzi", 7.5 }
  }
};

void catalog(struct Fish fish) {
  printf("%s is a %s with %hd teeth. It is %hd years old.\n",
      fish.name, fish.species, fish.teeth, fish.age);
}

void label(struct Fish fish) {
  printf("Name: %s\nSpecies: %s\n%hd years old,\n%hd teeth\n",
      fish.name, fish.species, fish.teeth, fish.age);

  printf("Feed with %2.2f lbs of %s and allow to %s for %2.2f hours.\n",
      fish.care.food.weight,
      fish.care.food.ingredients,
      fish.care.exercise.description,
      fish.care.exercise.duration);
}

int main(void) {
  catalog(snappy);
  label(snappy);

  return 0;
}

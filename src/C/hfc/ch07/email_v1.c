#include <stdio.h>

enum rtype { DUMP, CHANCE, MARRIAGE };

typedef struct Response_ {
  char *name;
  enum rtype type;
} Response;

void dump(Response r)
{
  printf("\nDear %s,\n", r.name);
  puts("Unfortunately your last date contacted us to");
  puts("say that they will not be seeing you again");
}

void chance(Response r)
{
  printf("\nDear %s,\n", r.name);
  puts("Good news: your last date has asked us to");
  puts("arrange another meeting. Please call ASAP.");
}

void marriage(Response r)
{
  printf("\nDear %s,\n", r.name);
  puts("Congratulations! Your last date has contacted");
  puts("us with a proposal of marriage.");
}

int main(void) {
  Response r[] = {
    { .name = "Mike", .type = DUMP },
    { .name = "Louis", .type = CHANCE },
    { .name = "Matt", .type = CHANCE },
    { .name = "Will", .type = MARRIAGE },
  };

  short i;

  for (i = 0; i < 4; ++i)
    switch (r[i].type) {
      case DUMP:
        dump(r[i]);
        break;
      case CHANCE:
        chance(r[i]);
        break;
      default:
        marriage(r[i]);
    }

  return 0;
}

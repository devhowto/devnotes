#include <stdio.h>
#include <string.h>

const short NUM_ADS = 7;

char *ADS[] = {
  "William: SBM GSOH likes sports, TV, dining",
  "Matt: SWM NS likes art, movies, theater",
  "Luis: SLM ND likes books, theater, art",
  "Mike: DWM DS likes trucks, sports and bieber",
  "Peter: SAM likes chess, working out and art",
  "Josh: SJM likes sports, movies and theater",
  "Jed: DBM likes theater, books and dining"
};

short sports_no_bieber(char *s) {
  return strstr(s, "sports") && !strstr(s, "bieber");
}

short sports_or_workout(char *s) {
  return strstr(s, "sports") || strstr(s, "working out");
}

short ns_theater(char *s) {
  return strstr(s, "NS") && strstr(s, "theater");
}

short arts_theater_or_dining(char *s) {
  return strstr(s, "arts")
    || strstr(s, "theater")
    || strstr(s, "dining");
}

void find(short (*match)(char *str)) {
  short i;

  puts("\n\nSearch results:");
  puts("--------------------------------------------");

  for (i = 0; i < NUM_ADS; ++i) {
    if (match(*(ADS + i))) {
      printf("%s\n", ADS[i]);
    }
  }

  puts("--------------------------------------------");
}

int main(void) {
  find(sports_no_bieber);
  find(sports_or_workout);
  find(ns_theater);
  find(arts_theater_or_dining);

  return 0;
}

/*
 * NOTE: We could have used & when passing the function as param
 * and also used * when dereferencing match.
 *
 *
 *    for (i = 0; i < NUM_ADS; ++i) {
 *      if ((*match)(*(ADS + i))) {
 *          --------
 *              \
 *               \
 *                +-> Note the * and parentheses (*match).
 *
 *        printf("%s\n", ADS[i]);
 *      }
 *    }
 *
 * And pass like find(&sports_or_workout) with the &.
 */

#include <stdio.h>

typedef union LemonLime_ {
  float lemon;
  short lime_pieces;
} LemonLime;

typedef struct Margarita_ {
  float tequila;
  float cointreau;
  LemonLime citrus;
} Margarita;

int main(void) {
  Margarita m1 = { 2.0, 1.0, { 2 } };
  Margarita m2 = { 2.0, 1.0, { 0.5 } };

  printf(
      "\n\n"
      "%2.1f measures of tequila\n"
      "%2.1f measures of cointreau\n"
      "%2.1f measures of juice\n",
      m1.tequila, m1.cointreau, m1.citrus.lemon);

  printf(
      "\n"
      "%2.1f measures of tequila\n"
      "%2.1f measures of cointreau\n"
      "%2.1f measures of juice\n",
      m2.tequila, m2.cointreau, m2.citrus.lemon);

  /* Doesn't work to use any field of union which is not the first. */
  // Margarita m3 = { 2.0, 1.0, { 0.5 } };

  /* But both of these next two likes is OK in >= C99. */
  // Margarita m3 = { 2.0, 1.0, { .lime_pieces = 1 } };
  Margarita m3 = { 2.0, 1.0, .citrus.lime_pieces = 1 };

  printf(
      "\n"
      "%2.1f measures of tequila\n"
      "%2.1f measures of cointreau\n"
      "%hd measures of juice\n",
      m3.tequila, m3.cointreau, m3.citrus.lime_pieces);
      //                       lime_pieces is NOT the first
      //                       field of the unition.

  return 0;
}

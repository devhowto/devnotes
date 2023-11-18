#include <stdio.h>

/**
 * (origin)
 *     ·
 *       ·
 *         ·
 *           ·
 *             ·
 *        (destination)
 *
 * Moving south-east means:
 *
 *   • latitude decreases
 *   • longitude increases
 */
void go_south_east (short lat, short lon)
{
  lat = lat - 1;
  lon = lon + 1;
}

int main ()
{
  short latitude = 0;
  short longitude =   0;

  go_south_east (latitude, longitude);

  fprintf (stdout,
           "Avast! Now at: [%i, %i]\n",
           latitude, longitude);

  return 0;
}

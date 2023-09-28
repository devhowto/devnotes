#include <stdio.h>

struct Point {
  short x;
  short y;
};

short move(struct Point);

short move(struct Point p) {
  return p.x - p.y;
}

int main(void) {
  short pos = move((struct Point){.x = 1, .y = 2});

  printf("Position: %hd\n", pos);

  return 0;
}

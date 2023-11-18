#include <stdio.h>
#include <stdlib.h>

void fn(int cnt) {
  int res = 5;
  printf("  fn 1 %p\n", (void *) &res);
  printf("  fn 2 %p\n", (void *) &cnt);
  printf("  fn 2 %p\n", (void *) __builtin_frame_address(0));
}

int main() {
  int i;

  printf("main 1 %p\n", (void *) __builtin_frame_address(0));
  printf("main 2 %p\n", (void *) &i);

  fn(3);
}

#include <stdio.h>
#include <stdarg.h>

/*
 * “va” probably stands for “variadic arguments”.
 * “ap” probably stands for “argument parameters”.
 */

void print_ints(int num_args, ...) {
  va_list ap;
  va_start(ap, num_args);
  short i;

  for (i = 0; i < num_args; ++i)
    printf("Argument: %i\n", va_arg(ap, int));

  va_end(ap);
}

int main(void) {
  print_ints(3, 10, 20, 30);

  return 0;
}

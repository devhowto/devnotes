#include <stdio.h>

void str_rev(char* src, char* dst, short len) {
  short i = 0;
  short j = len - 1;

  *(dst + (len - 1)) = '\0';

  while (i < len)
    *(dst + j--) = *(src + i++);
}

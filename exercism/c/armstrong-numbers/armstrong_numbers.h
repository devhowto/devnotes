#ifndef ARMSTRONG_NUMBERS_H
#define ARMSTRONG_NUMBERS_H

#include <stddef.h>
#include <stdbool.h>

size_t count_digits(long int n);
short* to_digits(long int n, size_t *len);
bool is_armstrong_number(int candidate);

#endif

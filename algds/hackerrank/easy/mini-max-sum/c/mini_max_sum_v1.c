#include <stdio.h>
#include <criterion/criterion.h>

struct MinMax {
  short min;
  short max;
};



struct MinMax mini_max_sum(short xs[], short len) {
  struct MinMax minmax;
  short min = *xs;
  short max = *xs;
  short total = *xs;

  for (short int i = 1; i < len; ++i) {
    total += *(xs + i);

    if (*(xs + i) < min)
      min = *(xs + i);

    if (*(xs + i) > max)
      max = *(xs + i);
  }

  minmax.min = total - max;
  minmax.max = total - min;

  return minmax;
}

void tester(short input[], short len, struct MinMax expected);

Test(Mini_Max_Sum_Tests, should_pass) {
  struct MinMax rmm;
  rmm.min = 10;
  rmm.max = 14;

  tester((short[]) {1, 2, 3, 4, 5}, 5, rmm);
}

void tester(short input[], short len, struct MinMax expected) {
  struct MinMax result = mini_max_sum(input, len);

  if (result.min != expected.min || result.max != expected.max) {
    cr_assert_fail(
      "Submitted min: %hd, max: %hd\nExpected min: %hd, max: %hd\n",
      result.min, result.max, expected.min, expected.max
    );
  }

  cr_assert(1);
}

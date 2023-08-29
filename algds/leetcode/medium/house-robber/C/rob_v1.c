#include <stdio.h>

#define MAX(x, y) (((x) > (y)) ? (x) : (y))

int rob(int* nums, int len) {
  int tmp;
  int max1 = 0;
  int max2 = 0;

  for (int i = 0; i < len; ++i) {
    tmp = MAX(max1, *(nums + i) + max2);
    max2 = max1;
    max1 = tmp;
  }

  return max1;
}

int main(void) {
  printf("Hey\n");

  int nums1[] = { 7 };
  int nums2[] = { 3, 7 };
  int nums3[] = { 1, 2, 5 };
  int nums4[] = { 1, 2, 3, 1 };
  int nums5[] = { 2, 7, 9, 3, 1 };

  printf("%d\n", rob(nums1, 1));
  //=> 7

  printf("%d\n", rob(nums2, 2));
  //=> 7

  printf("%d\n", rob(nums3, 3));
  //=> 6

  printf("%d\n", rob(nums4, 4));
  //=> 4

  printf("%d\n", rob(nums5, 5));
  //=> 12

  return 0;
}

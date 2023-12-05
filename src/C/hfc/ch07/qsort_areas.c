#include <stdio.h>
#include <stdlib.h>

typedef struct Rect_ {
  int width;
  int height;
} Rect;

int compare_area_asc(const void* rect_a, const void* rect_b) {
  /* Convert void pointers to concrete, correct types. */
  Rect *ra = (Rect*)rect_a;
  Rect *rb = (Rect*)rect_b;

  /* Then compute the areas */
  int area_a = (ra->width * ra->height);
  int area_b = (rb->width * rb->height);

  /* Apply the comparison */
  return area_a - area_b;
}

/* This works with less intermediate steps. */
/*
int compare_area_desc(const void* rect_a, const void* rect_b) {
  int area_a = (*(Rect*)rect_a).width * (*(Rect*)rect_a).height;
  int area_b = (*(Rect*)rect_b).width * (*(Rect*)rect_b).height;

  return area_b - area_a;
}
*/

/**
 * Implement the desc version in terms of the asc one just
 * by flipping the params order. */
int compare_area_desc(const void *rect_a, const void *rect_b) {
  return compare_area_asc(rect_b, rect_a);
}

int main(void) {
  Rect rect_1 = { .width = 4, .height = 2 }; // 8
  Rect rect_2 = { .width = 3, .height = 2 }; // 6
  Rect rect_3 = { .width = 2, .height = 1 }; // 2
  Rect rect_4 = { .width = 3, .height = 3 }; // 9

  Rect rects[4] = { rect_1, rect_2, rect_3, rect_4 };

  qsort(&rects, 4, sizeof(Rect), compare_area_asc);
  for (short i = 0; i < 4; ++i)
    printf("%d\n", rects[i].width * rects[i].height);

  printf("\n");

  qsort(&rects, 4, sizeof(Rect), compare_area_desc);
  for (short i = 0; i < 4; ++i)
    printf("%d\n", rects[i].width * rects[i].height);

  return 0;
}

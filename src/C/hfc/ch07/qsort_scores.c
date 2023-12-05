#include <stdio.h>
#include <stdlib.h>

int compare_scores_asc(const void* score_a, const void* score_b) {
  int a = *(int*)score_a;
  int b = *(int*)score_b;

  return a - b;
}

int compare_scores_desc(const void* score_a, const void* score_b) {
  int a = *(int*)score_a;
  int b = *(int*)score_b;

  return b - a;
}

int main(void) {
  int scores[7] = { 432, 5, 99134, 11, 9593, 741987, 1037234 };

  qsort(scores, 7, sizeof(int), compare_scores_asc);
  for (int i = 0; i < 7; ++i)
    printf("%d\n", scores[i]);

  puts("\n");

  qsort(scores, 7, sizeof(int), compare_scores_desc);
  for (int i = 0; i < 7; ++i)
    printf("%d\n", scores[i]);

  puts("\n");

  return 0;
}

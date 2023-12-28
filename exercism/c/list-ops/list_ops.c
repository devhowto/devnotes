#include <stdio.h>
#include "list_ops.h"

list_t *new_list(size_t len, list_element_t elmts[]) {
  list_t *list = malloc(sizeof(list_t) + sizeof(list_element_t) * len);
  list->length = len;

  /* Sometimes we'll have a length > 0 but will add elements later
   * by some other means, like concatenating two lists. It means
   * length will be > 0 but elmts will be empty or NULL. */
  if (!elmts)
    return list;

  for (size_t i = 0; i < len; ++i)
    list->elements[i] = elmts[i];

  return list;
}

void delete_list(list_t *list) {
  free(list);
}

list_t *append_list(list_t *xs, list_t *ys) {
  size_t new_list_len = xs->length + ys->length;

  list_t *list = new_list(new_list_len, NULL);

  for (size_t i = 0; i < ys->length; ++i)
    list->elements[i] = ys->elements[i];

  return list;
}

// int main(void) {
//   list_t *l = new_list(3, (int[]){ 1, 2, 3 });
//
//   for (size_t i = 0; i < 3; ++i)
//     printf("%d\n", l->elements[i]);
//
//   free(l);
//
//   return 0;
// }

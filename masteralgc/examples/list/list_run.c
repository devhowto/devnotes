#include <stdio.h>
#include <stdio.h>
#include "list.h"

int main(void) {
  List list;

  list_init(&list, free);

  printf("size: %d\n", list.size);

  int *x = (void *)malloc(sizeof(int));
  *x = 42;
  list_ins_next(&list, NULL, x);
  int *x_data = list_data(list.head);
  printf("%d\n", *x_data);

  int *y = (void *)malloc(sizeof(int));
  *y = 100;
  ListElmt *head = list_head(&list);
  list_ins_next(&list, head, y);
  printf("size: %d\n", list.size);
  int *y_data = (void *)malloc(sizeof(int));

  *y_data = list_data(list.head->next);

  printf("y_data: %d\n", *y_data);

  return 0;
}

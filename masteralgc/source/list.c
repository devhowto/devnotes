#include <stdlib.h>
#include <string.h>

#include "list.h"

void list_init(List *list, void (*destroy)(void *data)) {
  list->size = 0;
  list->destroy = destroy;
  list->head = NULL;
  list->tail = NULL;

  return;
}

// void list_destroy(List *list) {
//   void *data;
//
//   while (list_size(list) > 0)
//     if (
//         list_rem_next(list, NULL, (void **)&data) == 0
//         && list->destroy != NULL
//        )
//       list->destroy(data);
//
//   memset(list, 0, sizeof(List));
//
//   return;
// }

int list_ins_next(List *list, ListElmt *elmt, const void *data) {
  ListElmt            *new_elmt;

  if ((new_elmt = (ListElmt *)malloc(sizeof(ListElmt))) == NULL)
    return -1;

  new_elmt->data = (void *)data;

  if (elmt == NULL) {
    // Empty list, insert at head.

    if (list_size(list) == 0)
      list->tail = new_elmt;

    new_elmt->next = list->head;
    list->head = new_elmt;
  }

  list->size++;

  return 1;
//   else {
//     // List not empty. Insert somewhere else.
//
//     if (elmt->next == NULL)
//       list->tail = new_elmt;
//
//     new_elmt->next = elmt->next;
//     elmt->next = new_elmt;
//   }
//
//   list->size++;
//
//   return 0;
}

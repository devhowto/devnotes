#ifndef LIST_H
#define LIST_H

#include <stdlib.h>

typedef struct ListElmt_ {
  void              *data;
  struct  ListElmt_ *next;
} ListElmt;

typedef struct List_ {
  int               size;
  int               (*match)(const void *key1, const void *key2);
  void              (*destroy)(void *data);
  ListElmt          head;
  ListElmt          tail;
} List;

/* Public interface */

void list_init(List *list, void (*destroy)(void *data));

void list_destroy(List *list);

int list_ins_next(List *list, ListElmt *elmt, const void *data);

int list_rem_next(List *list, ListElmt *elmt, const void **data);

#define list_size(list) ((list)->size)

#define list_head(list) ((list)->head)

#define list_tail(list) ((list)->tail)

#define list_is_head(list, elmt) ((elmt) == (list)->head ? 1 : 0)

#define list_is_tail(list, elmt) ((elmt)->next == NULL ? 1 : 0)

#define list_data(elmt) ((elmt)->data)

#define list_next(elmt) ((elmt)->next)

#endif

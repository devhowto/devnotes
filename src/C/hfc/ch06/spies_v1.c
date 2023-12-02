#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Node_ {
  char *question;
  struct Node_ *no;
  struct Node_ *yes;
} Node;

int yes_no(char *question) {
  char answer[3];

  printf("%s? (y/n): ", question);
  fgets(answer, 3, stdin);

  return answer[0] == 'y';
}

Node* create(char *question) {
  Node *n = malloc(sizeof(Node));

  n->question = strdup(question);
  n->no = NULL;
  n->yes = NULL;

  return n;
}

void release(Node *n) {
  if (n) {
    if (n->no) release(n->no);
    if (n->yes) release(n->yes);
    if (n->question) free(n->question);
    free(n);
  }
}

int main(void) {
  char question[80];
  char suspect[24];

  Node *start_node = create("Suspect has mustache");

  start_node->no = create("Loretta");
  start_node->yes = create("Vinny");

  Node *current;

  do {
    current = start_node;

    while (1) {
      if (yes_no(current->question)) {
        if (current->yes) {
          current = current->yes;
        } else {
          printf("SUSPECT IDENTIFIED\n");
          break;
        }
      }
      else if (current->no) {
        current = current->no;
      } else {
        /* Make the yes-node the new suspect name. */
        printf("Who's the suspect? ");
        fgets(suspect, 24, stdin);
        Node *yes_node = create(suspect);
        current->yes = yes_node;

        /* Make the no-node a copy of this question. */
        Node *no_node = create(current->question);
        current->no = no_node;

        /* Then replace the question with the new question. */
        printf("Question that is TRUE for %s but not for %s\ninput: ",
            suspect, current->question);
        fgets(question, 80, stdin);

        /* Free old storage in the heap before assigning a new value. */
        free(current->question);

        current->question = strdup(question);
      }
    }
  } while (yes_no("Run again"));

  release(start_node);

  return 0;
}

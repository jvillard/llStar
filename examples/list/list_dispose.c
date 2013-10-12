#include <stdlib.h>

struct node {
  int data;
  struct node *next;
};

void list_dispose(struct node *l) {
  struct node *cur, *prev;

  cur = prev = l;

  while (cur) {
    prev = cur;
    cur = cur->next;
    free(prev);
  }
}

#include <stdlib.h>

struct node {
  int data;
  struct node *next;
};

void list_dispose(struct node *l) {
  struct node *prev=l;
  while (l) {
    prev = l;
    l = l->next;
    free(prev);
  }
}

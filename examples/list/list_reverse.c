#include <stdlib.h>

struct node {
  int data;
  struct node *next;
};

struct node * list_reverse(struct node *l)  {
 struct node *a = NULL;
 struct node *b = NULL;
 struct node *c = NULL;
 a = l, b = NULL;
 
 while(a != NULL) {
  c = b, b = a, a = a->next;
  b->next = c;
 }
 
 return b;
}

#include <stdlib.h>

struct node {
  int data;
  struct node *left;
  struct node *right;
};

struct node *copytree(struct node *x) {
  if (!x) return NULL;
  
  struct node *l, *r, *y;
  l = copytree(x->left);
  r = copytree(x->right);
  y = malloc(sizeof(struct node));
  y->left = l; y->right = r;
  return y;
}

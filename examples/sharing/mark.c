struct node {
  int color;
  struct node *left;
  struct node *right;
};

void mark(struct node *x) {
  if (!x || x->color) return;
  x->color = 1;
  mark(x->left);
  mark(x->right);
}

void mark_trickier(struct node *x) {
  if (!x || x->color) return;
  x->color = 1;
  struct node *tmp = x->left;
  x->left = x-> right;
  x->right = tmp;
  mark(x->left);
  tmp = x->left;
  x->left = x-> right;
  mark(x->right);
  x->right = tmp;
}

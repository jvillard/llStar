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

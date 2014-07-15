struct node {
  int color;
  struct node *left;
  struct node *right;
};

void mark(struct node *x) {
  if (!x || x->color) return;
  mark(x->right);
  mark(x->left);
 mark(x->left);
  x->color = 1;
}

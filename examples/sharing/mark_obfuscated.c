struct node {
  int color;
  struct node *left;
  struct node *right;
};

void mark_trickier(struct node *x) {
  if (!x || x->color) return;
  x->color = 1;
  struct node *tmp = x->left;
  x->left = x-> right;
  x->right = tmp;
  mark_trickier(x->left);
  tmp = x->left;
  x->left = x-> right;
  mark_trickier(x->right);
  x->right = tmp;
}

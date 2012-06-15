struct node {
  int data;
  struct node *next;
};

void traverse(struct node *l) {
  struct node *cur;

  cur = l;

  while (cur)
    cur = cur->next;
}

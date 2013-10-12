struct node {
  int data;
  struct node *next;
};


struct node * list_append(struct node *l1, struct node *l2) {
  struct node *cur, *prev;

  prev = l1;
  cur = l1->next;

  while (cur) {
    prev = cur;
    cur = cur->next;
  }

  prev->next = l2;
  return l1;
}

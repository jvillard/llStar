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

/*
struct node * append(struct node *l1, struct node *l2) {
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
*/

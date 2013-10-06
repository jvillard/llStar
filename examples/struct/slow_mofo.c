#include <stdlib.h>
struct two {
  int a;
  int b;
};

struct ole {
  int *data;
  struct ole *next;
};

int chew_slowly(int a) {
  struct ole *p, s;
  int b;
  struct two *t;

  s.data = &b;

  p = malloc(sizeof(struct ole));
  if (!p) return a;
  p->data = malloc(sizeof(int));
  if (!(p->data)) {
    free(p);
    return a;
  }
  /* p->next = p; */ /* this is equivalent to the line below */
  *((struct ole **)((void *)p+sizeof(int *))) = p;
  *(p->next->next->next->data) = a;

  b = *((*p).data);
  
  free(p->data);


  t = malloc(sizeof(struct two));
  if (t) {
    t->a = 4444;
    t->b = 4445;
    free(t);
  }

  free(p);
  return b;
}

int main(void) {
  int n;
  
  if (chew_slowly(n) != n)
    return 0;
  else goto ERROR;

 ERROR: return 1;
}

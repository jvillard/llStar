#include <stdlib.h>
struct ole {
  int *data;
  struct ole *next;
};

int pointer_arith(int a) {
  struct ole *p;
  int b;

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
  free(p);
  return b;
}

int main(void) {
  int n;
  
  if (pointer_arith(n) == n)
    return 0;
  else goto ERROR;

 ERROR: return 1;
}

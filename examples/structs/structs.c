// clang -O0 -emit-llvm structs.c -c -o structs.bc
#include <stdlib.h>

struct two {
  int a;
  int b;
};

struct ole {
  int *data;
  struct ole *next;
};

int f(int a) {
  struct ole *p, s;
  int b;
  struct two *t;

  s.data = &b;

  p = malloc(sizeof(struct ole));
  p->data = malloc(sizeof(int));
  p->next = p;
  *(p->next->data) = a;

  b = *((*p).data);
  
  free(p->data);


  t = malloc(sizeof(struct two));
  t->a = 4444;
  t->a = 4445;

  free(t);
  free(p);
  return b;
}

int main() {
  int x,*y;

  x = f(1664);

  
  y = malloc(sizeof(int));
  *y = 1665;
  free(y);

  return x;
}

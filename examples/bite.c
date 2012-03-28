// clang -O0 -emit-llvm bite.c -c -o bite.bc
#include <stdlib.h>

struct two {
  int a;
  int b;
};

struct prout {
  int *data;
  struct prout *next;
  struct two two;
};

int f(int a) {
  struct prout *p, s;
  int b;
  struct two *t;

  (&b)[0] = 999888777;
  
  s.data = &b;

  p = malloc(sizeof(struct prout));
  p->data = malloc(sizeof(int));
  *(p->data) = a;
  p->next = p;

  b = *((*p).data);
  
  free(p->data);


  t = &(p->two);
  t->a = 4444;

  free(p);
  return b;
}

int main() {
  int x,*y;

  x = 0;
  x = (int) (((float) x)+1.0f);

  x = f(1664);

  
  y = malloc(sizeof(int));
  *y = 1665;
  free(y);

  /*
  z = malloc(sizeof(struct prout));
  z->data = *y;
  free(z);
  */

  return x;
}

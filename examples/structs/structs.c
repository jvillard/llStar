// clang -O0 -emit-llvm bite.c -c -o bite.bc
#include <stdlib.h>

struct two {
  int a;
  int b;
};

struct prout {
  int *data;
  struct prout *next;
};

int f(int a) {
  struct prout *p, s;
  int b;
  struct two *t;

  s.data = &b;

  p = malloc(sizeof(struct prout));
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

struct ij {
  int i;
  int j;
};

struct oneint {
  int i;
};

void setint(int *i) {
  *i = 0;
}

void yay_int(struct ij s) {
  s.i = 4;
  setint(&(s.i));
  s.j = 0;
}

void setifield(struct oneint *s) {
  s->i = 0;
}

void yay_yay_int(struct ij s) {
  setifield((struct oneint *)&s);
  s.j = 0;
}

int main() {
  int x,*y;

  x = f(1664);

  
  y = malloc(sizeof(int));
  *y = 1665;
  free(y);

  return x;
}

// clang -O0 -emit-llvm bite.c -c -o bite.bc
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
  //  *((struct ole **)((void *)p+sizeof(int *))) = p;
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

struct ij yay_yay_int(struct ij s) {
  setifield((struct oneint *)&s);
  s.j = 0;
  return s;
}

int main() {
  int x,*y;
  struct ij s;

  x = f(1664);

  
  y = malloc(sizeof(int));
  *y = 1665;
  free(y);

  s.i = x;
  s = yay_yay_int(s);
  s.j = x;

  return s.j;
}
// clang -O0 -emit-llvm bite.c -c -o bite.bc
#include <stdlib.h>

void skip(void) {}

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
  if (!p) return a;
  p->data = malloc(sizeof(int));
  if (!(p->data)) {
    free(p);
    return a;
  }
  /* p->next = p; */ /* this is equivalent to the line below */
  *((struct ole **)((void *)p+sizeof(int *))) = p;
  *(p->next->data) = a;

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

void yay_yay_int(struct ij *s) {
  setifield((struct oneint *)s);
  s->j = 0;
}

int main() {
  int x,*y;
  struct ij s;

  x = f(1664);

  s.i = x;
  yay_yay_int(&s);
  s.j = x;

  return s.j;
}

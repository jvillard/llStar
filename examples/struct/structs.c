#include <stdlib.h>

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

void yay_yay_int2(struct ij *s) {
  setifield((struct oneint *)s);
}

int main() {
  int x = 1664,*y;
  struct ij s;

  s.i = x;
  yay_yay_int(&s);
  s.j = x;

  return s.j;
}

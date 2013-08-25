#include <stdlib.h>

struct ij {
  int i;
  int j;
};


int prout(void) {
  struct ij *s;

  s = malloc(sizeof(struct ij));
  if(!s) return 0;
  s->i = 3;
  s->i = 4;
  s->j = 5;
  *(int *)((void *)s + sizeof(int)) = 6;
  free(s);
  return (int) s + 12;
}

int caca(struct ij *s) {
  return s->i;
}

void setint(int *i) {
  *i = 0;
}

void yay_int(struct ij s) {
  s.i = 4;
  setint(&(s.i));
  s.j = 0;
}

void al(void) {
  struct ij *p;

  p = malloc(sizeof(struct ij));
  if (p) free(p);
}

int unreach(void) {
  int *p;
  
  while(1);

  return 0;
}

int array(int a[], int i) {
  return a[i];
}

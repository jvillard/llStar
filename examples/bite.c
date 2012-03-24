// clang -O0 -emit-llvm bite.c -c -o bite.bc
#include <stdlib.h>

struct prout {
  int data;
  struct prout *next;
};

int f(int a) {
  struct prout *p = NULL;
  return a;
}

int main() {
  int x,*y;

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

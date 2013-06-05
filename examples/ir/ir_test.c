int test_switch (int rand) {
  int x = 1;
  
  switch (rand) {
  case 0: x = 0; break;
  case 1: break;
  case 3: x = 2;
  case 2: x++; break;
  default:
    rand = 1;
  }
  return (x == rand);
}

unsigned int trunc_and_zext(unsigned int x) {
  return (unsigned int) ((unsigned long int) x);
}

int trunc_and_sext(int x) {
  return (int) ((long int) x);
}

unsigned int ptr_to_int_to_ptr(unsigned char x) {
  return (long int) (void *) x;
}

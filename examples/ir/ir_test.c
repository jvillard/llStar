int test_switch (int rand) {
  int x = 1;
  
  switch (rand) {
  case 0: x = 0; break;
  case 1: break;
  case 3: x = 2;
  case 2: x++; break;
  default:
    x = rand;
  }
  return x;
}

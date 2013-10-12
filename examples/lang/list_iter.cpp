#include <list>

int mult(std::list<int> l) {
  std::list<int>::iterator i;
  int res = 1;

  for (i = l.begin(); i != l.end(); ++i) {
    res = res * (*i);
  }

  return res;
}

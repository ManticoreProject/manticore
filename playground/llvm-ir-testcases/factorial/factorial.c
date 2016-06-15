#include <stdio.h>

int fib(int n) {
   if (n == 0) {
     return 1;
   } else {
     return n * fib(n-1);
   }
}

int main() {
  int res = fib(10);
  printf("result: %d\n", res);  
  return 0;
}

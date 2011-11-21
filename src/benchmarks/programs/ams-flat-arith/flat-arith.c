#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <stdint.h>

/* compile with -std=c99 */

static inline int arith(int n) {
  int a = (10*n*n + 7*n + 17) % 100;
  int b = (91*n*n + 8*n + 18) % 100;
  int c = (11*n*n + 9*n + 27) % 100;
  int d = (21*n*n + 2*n + 27) % 100;
  int e = (33*n*n + 3*n + 37) % 100;
  return (a+b+c+d+e);
}

int* go(int n) {
  int* arr = (int*)(malloc(n*sizeof(int)));
  for (int i=0; i<n; i++)
    arr[i] = arith(i);
  return arr;
}

void usage() {
  printf("usage: <executable name> [-size <num>]\n");
  exit(1);    
}

int sz = 10;

void args(int argc, char* argv[]) {
  int i=1;
  while (i<argc) {
    if (strcmp(argv[i], "-size") == 0) {
      i++;
      sz = atoi(argv[i]);
      if (sz <= 0) {
	printf("Please enter a positive integer size.\n");
	printf("You entered \"%s\".\n", argv[2]);
	exit(1);
      }
    } else {
      printf("Unrecognized option %s.\n", argv[i]);
      usage();
    }
    i++;
  } 
}

// timing utilities

int64_t now() {
  struct timeval now;
  gettimeofday(&now, 0);
  return 1000000 * now.tv_sec + now.tv_usec;
}

int64_t from_timeval(struct timeval tv) {
  return 1000000 * tv.tv_sec + tv.tv_usec;
}

int main(int argc, char* argv[]) {
  struct timeval t0;
  struct timeval t1;
  args(argc, argv);
  gettimeofday(&t0, 0);
  int* stuff = go(sz);
  gettimeofday(&t1, 0);
  int64_t t = from_timeval(t1) - from_timeval(t0);
  printf("%lld\n", t);
  free(stuff);
}

/* Adam Shaw 6/2011 */

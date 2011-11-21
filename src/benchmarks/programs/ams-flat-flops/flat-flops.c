#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <stdint.h>
#include <math.h>

/* compile with -std=c99 */

static inline double flops(double x) {
  double a = sin(2.0*x*x) + cos(7.0*x) / 17.0;
  double b = cos(3.0*x*x) + tan(8.0*x) / 18.0;
  double c = tan(4.0*x*x) + sqrt(9.0*x) / 12.0;
  double d = sqrt(5.0*x*x) + sin(1.1*x) / 27.0;
  double e = sin(6.0*x*x) + cos(3.1*x) / 37.0;
  return(a+b+c+d+e);
}

double* go(int n) {
  double* arr = (double*)(malloc(n*sizeof(double)));
  for (int i=0; i<n; i++)
    arr[i] = flops((double)i);
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
  double* stuff = go(sz);
  gettimeofday(&t1, 0);
  int64_t t = from_timeval(t1) - from_timeval(t0);
  printf("%lld\n", t);
  free(stuff);
}

/* Adam Shaw 6/2011 */

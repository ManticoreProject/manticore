#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <stdint.h>

/* compile with -std=c99 */

double xbase = -2.0;
double ybase = -2.0;
double zbase = -2.0;
double side = 4.0;

double magnitude(double x, double y, double z) {
  return sqrt(x*x+y*y+z*z);
} 

int lim = 1000;

int iter(int n, double x, double y, double z) {
  int i = 0;
  double cx = x;
  double cy = y;
  double cz = z;
  while (i <= lim) {
    double r = magnitude(cx,cy,cz);
    if (r >= 2.0) 
      return i;
    double phi = atan2(cy,cx);
    double theta = atan2(sqrt(cx*cx+cy*cy),cz);
    double nPhi = ((double)n)*phi;
    double nTheta = ((double)n)*theta;
    double r_n = pow(r, (double)n);
    cx = r_n * sin(nTheta) * cos(nPhi);
    cy = r_n * sin(nTheta) * sin(nPhi);
    cz = r_n * cos(nTheta);
    i++;
  }
  return lim;
}

int power = 8;

int*** mandelbulb(int n) {
  int *** arr = (int***)(malloc(n*sizeof(int**)));
  for (int i=0; i<n; i++) {
    arr[i] = (int**)(malloc(n*sizeof(int*)));
    for (int j=0; j<n; j++) {
      arr[i][j] = (int*)(malloc(n*(sizeof(int))));
      for (int k=0; k<n; k++) {
	double delta = side / ((double)(n-1));
	double cx = xbase + (delta * ((double)k));
	double cy = ybase + (delta * ((double)j));
	double cz = zbase + (delta * ((double)i));
	int count = iter(power,cx,cy,cz);
	/* printf("%d %d %d %lg %lg %lg %d\n", i, j, k, cx, cy, cz, count); */
	arr[i][j][k] = count;
      }
    }
  }
  return arr;
}

void usage() {
  printf("usage: <executable name> [-v] [-size <num>]\n");
  exit(1);    
}

int chatty = 0;
int sz = 64;

void args(int argc, char* argv[]) {
  int i=1;
  while (i<argc) {
    if (strcmp(argv[i], "-v") == 0) {
      chatty = 1;
    } else if (strcmp(argv[i], "-size") == 0) {
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

void show(int*** arr) {
  printf("[");    
  for (int i=0; i<sz; i++) {
    printf("[");
    for (int j=0; j<sz; j++) {
      printf("[");
      for (int k=0; k<sz; k++) {
	printf("%d ", arr[i][j][k]);
      }
      printf("]\n");
    }
    printf("]\n");
  }
  printf("]\n");
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
  int*** counts = mandelbulb(sz);
  gettimeofday(&t1,0);
  int64_t t = from_timeval(t1) - from_timeval(t0);
  printf("%lld\n", t);
  if (chatty) show(counts);
  free(counts);
}

/* Adam Shaw 5/2011 */

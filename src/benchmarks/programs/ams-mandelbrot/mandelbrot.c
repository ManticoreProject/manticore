#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <stdint.h>

/* compile with -std=c99 */

float xBase = -2.0;
float yBase = 1.25;
float side = 2.5;
int maxCount = 1000;

static inline int elt(int N, int i, int j) {
  float delta = side / ((float)(N-1));
  float c_re = xBase + (delta * ((float)j));
  float c_im = yBase - (delta * ((float)i));
  int count = 0;
  float z_re = c_re;
  float z_im = c_im;
  float z_re_sq = z_re * z_re;
  float z_im_sq = z_im * z_im;
  float z_re_im;  
  while ((count < maxCount) && (z_re_sq + z_im_sq < 4.0)) {
    z_re_im = z_re * z_im;
    count++;
    z_re = (z_re_sq - z_im_sq) + c_re;
    z_im = z_re_im + z_re_im + c_im;
    z_re_sq = z_re * z_re;
    z_im_sq = z_im * z_im;    
  }
  return count;
}

int** mandelbrot(int N) {
  int** arr = (int**)(malloc(N*sizeof(int*)));
  for (int i=0; i<N; i++) {
    arr[i] = (int*)(malloc(N*sizeof(int)));
    for (int j=0; j<N; j++) 
      arr[i][j] = elt(N,i,j); 
  }
  return arr;
}

void usage() {
  printf("usage: <executable name> [-v] [-size <num>]\n");
  exit(1);    
}

int chatty = 0;
int sz = 1024;

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

void show(int** arr) {
  printf("[");    
  for (int i=0; i<sz; i++) {
    printf("[");
    for (int j=0; j<sz; j++)
      printf("%d ", arr[i][j]);
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
  int** counts = mandelbrot(sz);
  gettimeofday(&t1, 0);
  int64_t t = from_timeval(t1) - from_timeval(t0);
  printf("%lld\n", t);
  if (chatty) show(counts);
  free(counts);
}

/* Adam Shaw 5/2011 */

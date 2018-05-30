


#include "treadmill.h"

#include <time.h>
#include <stdlib.h>


LargeObject_t** runMutator(Treadmill_t* tm, size_t numAllocs, LargeObject_t** roots, size_t maxRoots, int pctLive) {
  size_t numRoots = 0;
  for (size_t i = 0; i < numAllocs; i++) {
    uint8_t* mem = tm_alloc(tm);

    if (numRoots < maxRoots) {
      if ((rand() % 101) <= pctLive) {
        LargeObject_t* lo = (LargeObject_t*)(mem - offsetof(LargeObject_t, contents));
        *roots = lo;
        roots++;
        numRoots++;
      }
    }
  }
  *roots = NULL;
  return roots;
}

int main () {
  Treadmill_t* tm = malloc(sizeof(Treadmill_t));

  size_t maxRoots = 500;
  size_t maxAllocs = 50;
  LargeObject_t** roots = calloc(maxRoots+1, sizeof(LargeObject_t*));

  tm_init(tm, 0);
  srand(time(NULL));
  const int testIters = 2;

  // start test

  tm_show(tm);
  LargeObject_t** rootsEnd = roots;
  for(int i = 0; i < testIters; i++) {
    size_t allocs = maxAllocs; // rand() % (maxAllocs + 1);
    fprintf(stderr, "\n\n-- iteration %i, allocs = %zd --\n", i+1, allocs);

    runMutator(tm, allocs, rootsEnd, maxRoots, 100);
    tm_show(tm);

    tm_start_gc(tm, roots);
    tm_show(tm);

    fprintf(stderr, "\n---------------\n\n");
  }

  return 0;
}

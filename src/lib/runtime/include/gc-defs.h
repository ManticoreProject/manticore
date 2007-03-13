#ifndef _GC_DEFS_H
#define _GC_DEFS_H

#ifndef NOT_C_SOURCE
#include "manticore-types.h"

typedef Word_t Mant_t;

int numGCs;

#endif

#define HEAP_ALIGN      (1<<20)
#define HEAP_SIZE       HEAP_ALIGN
#define HEAP_SIZE_W     (HEAP_SIZE>>3)
#define HEAP_BASE_MASK  (~(2*HEAP_SIZE-1))

#endif

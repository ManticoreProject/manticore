#ifndef _GC_DEFS_H
#define _GC_DEFS_H

#include "manticore-types.h"

typedef Word_t Mant_t;

#define HEAP_ALIGN      (1<<15)
#define HEAP_SIZE       HEAP_ALIGN

#define HEAP_SIZE_W (HEAP_SIZE>>3)

#endif

#ifndef _CONTEXT_H
#define _CONTEXT_H

#if defined(ARCH_AMD64)
#include "amd64/manticore-regs.h"
#endif

typedef struct {
  Word_t gprs[NUM_GPRS];
  Word_t pc;
} Context_t;

#endif

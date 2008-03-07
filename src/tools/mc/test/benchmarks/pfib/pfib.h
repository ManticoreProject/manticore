#include "pfib-body-exp.h"

#define seq_fib()			       \
  fun fib (i) =                                \
    pfib_body(fib, val)			       \
;                                              \

#define par_fib(seq_cutoff)	               \
  fun pfib (i) = if (i < seq_cutoff)           \
      then fib(i)                              \
      else pfib_body(pfib,dval)	               \
;                                              \



/* atomic-ops.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various atomic operations used in the runtime. The operations are
 * as follows:
 *
 *	void *CompareAndSwap (void **ptr, void *key, void *new)
 *	{
 *	    void *old = *ptr;
 *	    if (old == key) {
 *		*ptr = new;
 *	    }
 *	    return old;
 *	}
 *
 *	int TestAndSwap (int *ptr, int new)
 *	{
 *	    int old = *ptr;
 *	    if (old == 0)
 *		*ptr = new;
 *	    return old;
 *	}
 *
 *      Value_t AtomicExchangeValue (Value_t *ptr, Value_t new)
 *      {
 *          Value_t result = *ptr;
 *          *ptr = new;
 *          return result;
 *      }
 *
 *      void AtomicWriteValue (Value_t *ptr, Value_t new)
 *      {
 *          *ptr = new;
 *      }
 *
 *	int FetchAndInc (int *ptr)
 *	{
 *	    int tmp = *ptr;
 *	    *ptr += 1;
 *	    return tmp;
 *	}
 *
 *	int FetchAndDec (int *ptr)
 *	{
 *	    int tmp = *ptr;
 *	    *ptr -= 1;
 *	    return tmp;
 *	}
 *
 *	int FetchAndAdd (int *ptr, int n)
 *	{
 *	    int tmp = *ptr;
 *	    *ptr += n;
 *	    return tmp;
 *	}
 */

#ifndef _ATOMIC_OPS_H_
#define _ATOMIC_OPS_H_

#if defined(__STDC_NO_ATOMICS__)
# error "C11 stdatomic support is required"
#endif

// The use of inline ASM prevents tools like TSan from detecting data races.

#include <stdatomic.h>

STATIC_INLINE Value_t CompareAndSwapValue (volatile Value_t *ptr, Value_t key, Value_t new)
{
    return __sync_val_compare_and_swap (ptr, key, new);
}

STATIC_INLINE Value_t CompareAndSwapValue_Atomic (volatile _Atomic Value_t *ptr, Value_t key, Value_t new)
{
    Value_t expected = key;
    atomic_compare_exchange_strong (ptr, &expected, new);
    return expected;
}

STATIC_INLINE Word_t CompareAndSwapWord_Atomic (volatile _Atomic Word_t *ptr, Word_t key, Word_t new)
{
    Word_t expected = key;
    atomic_compare_exchange_strong (ptr, &expected, new);
    return expected;
}

STATIC_INLINE Word_t CompareAndSwapWord (volatile Word_t *ptr, Word_t key, Word_t new)
{
    return __sync_val_compare_and_swap (ptr, key, new);
}

STATIC_INLINE void *CompareAndSwapPtr (void *volatile *ptr, void *key, void *new)
{
    return __sync_val_compare_and_swap (ptr, key, new);
}

STATIC_INLINE int TestAndSwap (volatile int *ptr, int new)
{
    return __sync_val_compare_and_swap (ptr, 0, new);
}

STATIC_INLINE Addr_t AtomicExchangeAddr (volatile _Atomic Addr_t *ptr, Addr_t new)
{
  return atomic_exchange(ptr, new);
}

STATIC_INLINE Value_t AtomicExchangeValue (volatile _Atomic Value_t *ptr, Value_t new)
{
  return atomic_exchange(ptr, new);
}

STATIC_INLINE void AtomicWriteValue (volatile _Atomic Value_t *ptr, Value_t new)
{
    atomic_store(ptr, new);
}

STATIC_INLINE Value_t AtomicReadValue (volatile _Atomic Value_t *ptr)
{
    return atomic_load(ptr);
}

STATIC_INLINE Word_t AtomicReadWord (volatile _Atomic Word_t *ptr)
{
    return atomic_load(ptr);
}

STATIC_INLINE int FetchAndInc (volatile int *ptr)
{
    return __sync_fetch_and_add(ptr, 1);
}

STATIC_INLINE int FetchAndDec (volatile int *ptr)
{
    return __sync_fetch_and_sub(ptr, 1);
}

STATIC_INLINE uint64_t FetchAndAdd64 (volatile int64_t *ptr, int64_t n)
{
    return __sync_fetch_and_add(ptr, n);
}

STATIC_INLINE int64_t FetchAndAddU64 (volatile uint64_t *ptr, uint64_t n)
{
    return __sync_fetch_and_add(ptr, n);
}


#endif /* !_ATOMIC_OPS_H_*/

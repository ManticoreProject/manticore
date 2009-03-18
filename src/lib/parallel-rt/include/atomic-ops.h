/* atomic-ops.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Various atomic operations used in the runtime.  If we are using
 * gcc 4.1+, then we use the compiler's builtin operations, otherwise
 * we have machine-specific assembly code.  The operations are
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
 *	bool BoolCompareAndSwap (void **ptr, void *key, void *new)
 *	{
 *	    if (*ptr == key) {
 *		*ptr = new;
 *		return true;
 *	    }
 *	    else
 *		return false;
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
 *      void AtomicWriteValue (Value_t *ptr, Value_t new)
 *      {
 *          *ptr = new;
 *          MemoryBarrier()    // flush all pending writes
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

#ifdef HAVE_BUILTIN_ATOMIC_OPS

STATIC_INLINE bool BoolCompareAndSwapValue (volatile Value_t *ptr, Value_t key, Value_t new)
{
    return __sync_bool_compare_and_swap (ptr, key, new);
}

STATIC_INLINE bool BoolCompareAndSwapWord (volatile Word_t *ptr, Word_t key, Word_t new)
{
    return __sync_bool_compare_and_swap (ptr, key, new);
}

STATIC_INLINE Value_t CompareAndSwapValue (volatile Value_t *ptr, Value_t key, Value_t new)
{
    return __sync_val_compare_and_swap (ptr, key, new);
}

STATIC_INLINE Word_t CompareAndSwapWord (volatile Word_t *ptr, Word_t key, Word_t new)
{
    return __sync_val_compare_and_swap (ptr, key, new);
}

STATIC_INLINE int TestAndSwap (volatile int *ptr, int new)
{
    return __sync_val_compare_and_swap (ptr, 0, new);
}

STATIC_INLINE void AtomicWriteValue (volatile Value_t *ptr, Value_t new)
{
    *ptr = new;
    __sync_synchronize();
}

STATIC_INLINE int FetchAndInc (volatile int *ptr)
{
    return __sync_fetch_and_add(ptr, 1);
}

STATIC_INLINE int FetchAndDec (volatile int *ptr)
{
    return __sync_fetch_and_sub(ptr, 1);
}

STATIC_INLINE int64_t FetchAndAdd64 (volatile int64_t *ptr, int64_t n)
{
    return __sync_fetch_and_add(ptr, n);
}

#else /* !HAVE_BUILTIN_ATOMIC_OPS */

STATIC_INLINE Value_t CompareAndSwapValue (volatile Value_t *ptr, Value_t old, Value_t new)
{
    Value_t result;

    __asm__ __volatile__ (
	"movq %2,%%rcx\n\t"		/* %rcx = new */
	"movq %1,%%rax\n\t"		/* %rax = old */
	"lock; cmpxchgq %%rcx,%3;\n\t"	/* cmpxchg %rcx,ptr */
        "movq %%rax,%0;\n"		/* result = %rax */
	    : "=r" (result)
 	    : "g" (old), "g" (new), "m" (*ptr)
  	    : "memory", "%rax", "%rcx");
    return result;
}

STATIC_INLINE Word_t CompareAndSwapWord (volatile Word_t *ptr, Word_t old, Word_t new)
{
    Word_t result;

    __asm__ __volatile__ (
	"movq %2,%%rcx\n\t"		/* %rcx = new */
	"movq %1,%%rax\n\t"		/* %rax = old */
	"lock; cmpxchgq %%rcx,%3;\n\t"	/* cmpxchg %rcx,ptr */
        "movq %%rax,%0;\n"		/* result = %rax */
	    : "=r" (result)
 	    : "g" (old), "g" (new), "m" (*ptr)
  	    : "memory", "%rax", "%rcx");
    return result;
}

STATIC_INLINE int TestAndSwap (volatile int *ptr, int new)
{
    int result;

    __asm__ __volatile__ (
	"movl %1,%%ecx\n\t"		/* %ecx = new */
	"xorl %%eax,%%eax\n\t"		/* %eax = 0 */
	"lock; cmpxchgl %%ecx,%2;\n\t"	/* cmpxchg %ecx,ptr */
	"movl %%eax,%0;\n"		/* result = %eax */
	    : "=r" (result)
	    : "g" (new), "m" (*ptr)
	    : "memory", "%eax");
    return result;
}

STATIC_INLINE void AtomicWriteValue (volatile Value_t *ptr, Value_t new)
{
    CompareAndSwapValue(ptr, *ptr, new);
}

STATIC_INLINE int FetchAndInc (volatile int *ptr)
{
    int		incr = 1;
    __asm__ __volatile__ (
	"lock; xaddl %0,%1\n"
	    : "=r" (incr), "=m" (*ptr)
	    : "0" (incr) : "memory");
    return incr;
}

STATIC_INLINE int FetchAndDec (volatile int *ptr)
{
    int		incr = -1;
    __asm__ __volatile__ (
	"lock; xaddl %0,%1\n"
	    : "=r" (incr), "=m" (*ptr)
	    : "0" (incr) : "memory");
    return incr;
}

STATIC_INLINE int64_t FetchAndAdd64 (volatile int64_t *ptr, int64_t n)
{
    int64_t	incr = n;
    __asm__ __volatile__ (
	"lock; xaddq %0,%1\n"
	    : "=r" (incr), "=m" (*ptr)
	    : "0" (incr) : "memory");
    return incr;
}

#endif /* HAVE_BUILTIN_ATOMIC_OPS */

#endif /* !_ATOMIC_OPS_H_*/

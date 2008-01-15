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
 */

#ifndef _ATOMIC_OPS_H_
#define _ATOMIC_OPS_H_

#ifdef HAVE_BUILTIN_ATOMIC_OPS

STATIC_INLINE bool BoolCompareAndSwapValue (Value_t *ptr, Value_t key, Value_t new)
{
    return __sync_bool_compare_and_swap (ptr, key, new);
}

STATIC_INLINE bool BoolCompareAndSwapWord (Word_t *ptr, Word_t key, Word_t new)
{
    return __sync_bool_compare_and_swap (ptr, key, new);
}

STATIC_INLINE Value_t CompareAndSwapValue (Value_t *ptr, Value_t key, Value_t new)
{
    return __sync_val_compare_and_swap (ptr, key, new);
}

STATIC_INLINE Word_t CompareAndSwapWord (Word_t *ptr, Word_t key, Word_t new)
{
    return __sync_val_compare_and_swap (ptr, key, new);
}

STATIC_INLINE int TestAndSwap (int *ptr, int new)
{
    return __sync_val_compare_and_swap (ptr, 0, new);
}

STATIC_INLINE int FetchAndInc (int *ptr)
{
    return __sync_fetch_and_add(ptr, 1);
}

STATIC_INLINE int FetchAndDec (int *ptr)
{
    return __sync_fetch_and_sub(ptr, 1);
}

#else /* !HAVE_BUILTIN_ATOMIC_OPS */

STATIC_INLINE Value_t CompareAndSwapValue (volatile Value_t *ptr, Value_t old, Value_t new)
{
  Value_t result;

    __asm__ __volatile__ (
	"movq %2,%%rbx\n\t"		/* %rbx = new */
	"movq %1,%%rax\n\t"		/* %rax = 0 */
	"lock; cmpxchgq %%rbx,%3;\n\t"	/* cmpxchg %rbx,ptr */
        "movq %%rax,%0;\n"
	    : "=r" (result)
 	    : "g" (old), "g" (new), "m" (*ptr)
  	    : "memory", "%rax", "%rbx");
    return result;
}

STATIC_INLINE Word_t CompareAndSwapWord (Word_t *ptr, Word_t old, Word_t new)
{
    register Word_t result __asm__ ("%rax");

    __asm__ __volatile__ (
	"movq %2,%%rbx\n\t"		/* %rbx = new */
	"movq %1,%%rax\n\t"		/* %rax = 0 */
	"lock; cmpxchgq %%rbx,%0;\n"	/* cmpxchg %rbx,ptr */
	    : "=m" (*ptr)
    	    : "g" (old), "g" (new)
	    : "memory", "%rax");
    return result;
}

STATIC_INLINE int TestAndSwap (int *ptr, int new)
{
    register int result __asm__ ("%eax");

    __asm__ __volatile__ (
	"movl %1,%%ebx\n\t"		/* %ebx = new */
	"xorl %%eax,%%eax\n\t"		/* %eax = 0 */
	"lock; cmpxchgl %%ebx,%0;\n"	/* cmpxchg %ebx,ptr */
	    : "=m" (*ptr)
	    : "g" (new)
	    : "memory", "%eax");
    return result;
}

STATIC_INLINE int FetchAndInc (int *ptr)
{
    int		incr = 1;
    __asm__ __volatile__ (
	"lock; xaddl %0,%1\n"
	    : "=r" (incr), "=m" (*ptr)
	    : "0" (incr) : "memory");
    return incr;
}

STATIC_INLINE int FetchAndDec (int *ptr)
{
    int		incr = -1;
    __asm__ __volatile__ (
	"lock; xaddl %0,%1\n"
	    : "=r" (incr), "=m" (*ptr)
	    : "0" (incr) : "memory");
    return incr;
}

#endif /* HAVE_BUILTIN_ATOMIC_OPS */

#endif /* !_ATOMIC_OPS_H_*/

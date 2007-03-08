#ifndef _BIBOP_H_
#define _BIBOP_H_

#include "manticore-rt.h"

#define PAGE_BITS	20	/* one-megabyte pages in the global heap */
#define BIBOP_PAGE_SZB	(1<<PAGE_BITS)

#ifdef SIXTYFOUR_BIT_WORDS

#define ADDR_BITS	48	/* on current 64-bit machines, virtual address are restricted */
				/* to 48 bits */
#define L1_BITS		14
#define L2_BITS		(ADDR_BITS-(L1_BITS+PAGE_BITS))
#define L1_SHIFT	(ADDR_BITS-L1_BITS)
#define L2_SHIFT	PAGE_BITS
#define L1_TBLSZ	(1 << L1_BITS)
#define L2_TBLSZ	(1 << L2_BITS)

extern MemChunk_t	**BIBOP[L1_TBLSZ];

STATIC_INLINE MemChunk_t *AddrToChunk (Addr_t addr)
{
    assert (addr < (1l << ADDR_BITS));
    return BIBOP[addr >> L1_SHIFT][addr >> L2_SHIFT];
}

#else /* !SIXTYFOUR_BIT_WORDS */

#define ADDR_BITS	32
#define BIBOP_BITS	(ADDR_BITS-PAGE_BITS)
#define BIBOP_TBLSZ	(1 << BIBOP_BITS)

extern MemChunk_t	*BIBOP[BIBOP_TBLSZ];

STATIC_INLINE MemChunk_t *AddrToChunk (Addr_t addr)
{
    return BIBOP[addr >> PAGE_BITS];
}

#endif /* SIXTYFOUR_BIT_WORDS */

#endif /* !_BIBOP_H_ */

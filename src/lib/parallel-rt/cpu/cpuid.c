/*! \file cpuid.c
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>

typedef enum {
	Intel,
	AMD
} Vendor_t;

typedef struct {
    uint16_t	level;	//!< cache level
    uint16_t	assoc;	//!< cache associativity
    uint16_t	lineSzB; //!< line size in bytes
    uint16_t	szKB;	//!< cache size in kilobytes
} CacheInfo_t;

// Leaf 2 cache and TLB information
static CacheInfo_t	CacheInfoTbl[256] = {
		// level, assoc, lineSzB, szKB
	[0x00] = { 0,  0,   0,     0 },	// Null descriptor
	[0x01] = { 0,  0,   0,     0 },	// TLB
	[0x02] = { 0,  0,   0,     0 },	// TLB
	[0x03] = { 0,  0,   0,     0 },	// TLB
	[0x04] = { 0,  0,   0,     0 },	// TLB
	[0x05] = { 0,  0,   0,     0 },	// TLB
	[0x06] = { 1,  4,  32,     8 }, // L1 I-cache
	[0x08] = { 1,  4,  32,    16 }, // L1 I-cache
	[0x09] = { 1,  4,  32,    32 }, // L1 I-cache
	[0x0a] = { 1,  4,  64,    32 }, // L1 D-cache
	[0x0b] = { 0,  0,   0,     0 },	// TLB
	[0x0c] = { 1,  4,  32,    16 }, // L1 D-cache
	[0x0d] = { 1,  4,  64,    16 }, // L1 D-cache
	[0x0e] = { 1,  4,  64,    24 }, // L1 D-cache
	[0x21] = { 2,  8,  64,   256 }, // L2 cache
	[0x22] = { 3,  4,  64,   512 }, // L3 cache; 2 lines per sector
	[0x23] = { 3,  8,  64,  1024 }, // L3 cache; 2 lines per sector
	[0x25] = { 3,  8,  64,  2048 }, // L3 cache; 2 lines per sector
	[0x29] = { 3,  8,  64,  4096 }, // L3 cache; 2 lines per sector
	[0x2c] = { 1,  8,  64,    32 }, // L1 D-cache
	[0x30] = { 1,  8,  64,    32 }, // L1 I-cache
	[0x40] = { 0,  0,   0,     0 }, // No L2 cache or no L3 cache
	[0x41] = { 2,  4,  32,   128 }, // L2 cache
	[0x42] = { 2,  4,  32,   256 }, // L2 cache
	[0x43] = { 2,  4,  32,   512 }, // L2 cache
	[0x44] = { 2,  4,  32,  1024 }, // L2 cache
	[0x45] = { 2,  4,  32,  2096 }, // L2 cache
	[0x46] = { 3,  4,  64,  4096 }, // L3 cache
	[0x47] = { 3,  8,  64,  8192 }, // L3 cache
	[0x48] = { 2, 12,  64,  3072 }, // L2 cache
	[0x49] = { 2, 16,  64,  4096 }, // L2 cache (or L3 on Xeon family 0xf, model 0x6)
	[0x4a] = { 3, 12,  64,  6144 }, // L3 cache
	[0x4b] = { 3, 16,  64,  8192 }, // L3 cache
	[0x4c] = { 3, 12,  64, 12288 }, // L3 cache
	[0x4d] = { 3, 16,  64, 16384 }, // L3 cache
	[0x4e] = { 2, 24,  64,  6144 }, // L2 cache
	[0x4f] = { 0,  0,   0,     0 },	// TLB
	[0x50] = { 0,  0,   0,     0 },	// TLB
	[0x51] = { 0,  0,   0,     0 },	// TLB
	[0x52] = { 0,  0,   0,     0 },	// TLB
	[0x55] = { 0,  0,   0,     0 },	// TLB
	[0x56] = { 0,  0,   0,     0 },	// TLB
	[0x57] = { 0,  0,   0,     0 },	// TLB
	[0x59] = { 0,  0,   0,     0 },	// TLB
	[0x5a] = { 0,  0,   0,     0 },	// TLB
	[0x5b] = { 0,  0,   0,     0 },	// TLB
	[0x5c] = { 0,  0,   0,     0 },	// TLB
	[0x5d] = { 0,  0,   0,     0 },	// TLB
	[0x60] = { 1,  4,  64,    16 }, // L1 D-cache
	[0x66] = { 1,  4,  64,     8 }, // L1 D-cache
	[0x67] = { 1,  4,  64,    16 }, // L1 D-cache
	[0x68] = { 1,  4,  64,    32 }, // L1 D-cache
	[0x70] = { 0,  0,   0,     0 }, // trace cache
	[0x71] = { 0,  0,   0,     0 }, // trace cache
	[0x72] = { 0,  0,   0,     0 }, // trace cache
	[0x78] = { 2,  4,  64,  1024 }, // L2 cache
	[0x79] = { 2,  8,  64,   128 }, // L2 cache
	[0x7a] = { 2,  8,  64,   256 }, // L2 cache
	[0x7b] = { 2,  8,  64,   512 }, // L2 cache
	[0x7c] = { 2,  8,  64,  1024 }, // L2 cache
	[0x7d] = { 2,  8,  64,  2096 }, // L2 cache
	[0x7f] = { 2,  2,  64,   512 }, // L2 cache
	[0x80] = { 2,  8,  64,   512 }, // L2 cache
	[0x82] = { 2,  8,  32,   256 }, // L2 cache
	[0x83] = { 2,  8,  32,   512 }, // L2 cache
	[0x84] = { 2,  8,  32,  1024 }, // L2 cache
	[0x85] = { 2,  8,  32,  2096 }, // L2 cache
	[0x86] = { 2,  4,  64,   512 }, // L2 cache
	[0x87] = { 2,  8,  64,  1024 }, // L2 cache
	[0xb0] = { 0,  0,   0,     0 },	// TLB
	[0xb1] = { 0,  0,   0,     0 },	// TLB
	[0xb2] = { 0,  0,   0,     0 },	// TLB
	[0xb3] = { 0,  0,   0,     0 },	// TLB
	[0xb4] = { 0,  0,   0,     0 },	// TLB
	[0xba] = { 0,  0,   0,     0 },	// TLB
	[0xc0] = { 0,  0,   0,     0 },	// TLB
	[0xca] = { 0,  0,   0,     0 },	// shared TLB
	[0xe4] = { 3, 16,  64, 16384 }, // L3 cache
	[0xf0] = { 0,  0,   0,     0 }, // 64-byte prefetch
	[0xf1] = { 0,  0,   0,     0 }, // 128-byte prefetch
	[0xff] = { 0,  0,   0,     0 }, // No cache info in leaf 2
    };

//! \brief CPUID info by register
typedef struct {
    uint32_t	eax;	//!< %eax contents
    uint32_t	ebx;	//!< %ebx contents
    uint32_t	ecx;	//!< %ecx contents
    uint32_t	edx;	//!< %edx contents
} CPUID_Regs_t;

//! \brief basic info (leaf 0)
typedef struct {
    uint32_t	max;	// maximum basic code
    char	id[12];	// vendor ID
} CPUID_Leaf0_t;

//! \brief cache and TLB info (leaf 2)
typedef struct {
    unsigned char eax[4];
    unsigned char ebx[4];
    unsigned char ecx[4];
    unsigned char edx[4];
} CPUID_Leaf2_t;

//! \brief deterministic cache info (leaf 4)
typedef struct {
  // EAX
    unsigned		cacheType : 5;
    unsigned		level : 3;
    unsigned		selfInit : 1;
    unsigned		fullAssoc : 1;
    unsigned		unused : 4;
    unsigned		maxThdId : 12;
    unsigned		maxCoreId : 6;
  // EBX
    unsigned		sysLineSz : 12;
    unsigned		lineSz : 10;
    unsigned		assoc : 10;
  // ECX
    uint32_t		numSets;
  // EDX
    unsigned		wrinvd : 1;
    unsigned		inclusive : 1;
    unsigned		unused2 : 30;
} CPUID_Leaf4_t;

typedef struct {
    uint32_t	op;	//!< the operation
    union {
	CPUID_Regs_t	regs;
	uint32_t	data[4];
	CPUID_Leaf0_t	basicInfo;
	CPUID_Leaf2_t	cacheInfo;
	CPUID_Leaf4_t	cacheParams;
    }		u;
} CPUID_t;

// forward declarations of local functions
static void CPUID (uint32_t leaf, uint32_t op2, CPUID_t *info);


/***** Local functions *****/
	
/*! \brief invoke the CPUID instruction with the given operation
 *	   and return the values returned in %eax, %ebx, %ecx, %edx.
 *  \param leaf specifies which leaf of the info tree we want.
 *  \param op2 secondary operation, which is used by some leaves
 *  \param info pointer to result struct.
 */
static void CPUID (uint32_t leaf, uint32_t op2, CPUID_t *info)
{
    uint64_t	saveRBX;	// %rbx is callee-save

    asm ("movq %%rbx,%0" : "=m" (saveRBX));

    info->op = leaf;

  // load operation code into %eax
    asm ("movl %0,%%eax; " : : "m" (leaf));
    asm ("movl %0,%%ecx; " : : "m" (op2));
    asm ("cpuid" : : : "%eax", "%ecx", "%edx");
    asm (
	"movl %%eax,%0; movl %%ebx,%1; movl %%ecx,%2; movl %%edx,%3; "
	: "=m" (info->u.regs.eax), "=m" (info->u.regs.ebx),
	  "=m" (info->u.regs.ecx), "=m" (info->u.regs.edx)
	:
	: "%eax", "%edx", "%ecx");
    asm ("movq %0,%%rbx" : : "m" (saveRBX));

}


#ifdef TEST_CPUID

/***** Test code *****/

int main (int argc, char **argv)
{
    CPUID_t	info;
    CPUID_Leaf4_t *p = &(info.u.cacheParams);

  // get vendor info
    CPUID (0, 0, &info);
    char buf[16];
    strncpy (buf, info.u.basicInfo.id, 4);
    strncpy (buf+4, info.u.basicInfo.id+8, 4);
    strncpy (buf+8, info.u.basicInfo.id+4, 4);
    printf("vendor = %s\n", buf);
    printf("max CPUID code = %d\n", info.u.basicInfo.max);

    int i = 0;
    do {
	CPUID (4, i, &info);
	if (p->cacheType != 0) {
	    printf("***** %d *****\n", i++);
	    printf("  max cores      = %d\n", p->maxCoreId + 1);
	    printf("  max threads    = %d\n", p->maxThdId + 1);
	    printf("  cache type     = level %d %s%s\n",
		p->level,
		(p->cacheType == 1) ? "data"
		: ((p->cacheType == 2) ? "instruction" : "unified"),
		p->inclusive ? " (inclusive)" : "");
	    printf("  associativity  = %d way\n", p->assoc + 1);
	    printf("  line size      = %d\n", p->sysLineSz + 1);
	    printf("  num partitions = %d\n", p->lineSz + 1);
	    printf("  num sets       = %d\n", p->numSets + 1);
	}
    } while (p->cacheType != 0);

    return 0;

}

#endif

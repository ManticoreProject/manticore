/* manticore-rt.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file collects together all of the type definitions related to the
 * Manticore/run-time interface.
 */

#ifndef _MANTICORE_RT_H_
#define _MANTICORE_RT_H_

#include "manticore-config.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
/* #include "machine/manticore-regs.h" */
#include "header-bits.h"

/******************** Default sizes ********************/

#define ONE_K			1024
#define ONE_MEG			(ONE_K*ONE_K)

#include "machine/sizes.h"

#ifndef DFLT_TIME_Q_MS
#  define DFLT_TIME_Q_MS	100
#endif

#ifndef DFLT_NUM_VPROCS
#  define DFLT_NUM_VPROCS	2
#endif

/******************** Utility definitions ********************/

typedef uint8_t Byte_t;

#define MAX_WORD		((Word_t)(~0))
#define WORD_SZB		SIZEOF_CHAR_P
#if (WORD_SZB == 8)
#  define SIXTYFOUR_BIT_WORDS
#  define WORD_LOGSZB		3
#else
#  error "word size must be 64 bits"
#endif
#define BYTES_TO_WORDS(N)	(((N) + (WORD_SZB-1)) >> WORD_LOGSZB)

/* round up to a power-of-2 size */
#define ROUNDUP(N, POW_OF_TWO)	(((N) + (POW_OF_TWO-1)) & ~(POW_OF_TWO-1))

/* round down to a power-of-2 size */
#define ROUNDDOWN(N, POW_OF_TWO) ((N) & ~(POW_OF_TWO-1))

/* object allocation in the C heap */
#define MALLOC(sz)		malloc(sz)
#define CALLOC(n,sz)		calloc(n,sz)
#define REALLOC(p,sz)		realloc(p,sz)
#define FREE(p)			free(p)
#define NEW(ty)			((ty *)MALLOC(sizeof(ty)))
#define NEWCLR(ty)		((ty *)CALLOC(1, sizeof(ty)))
#define CHKNEW(x, ty)		((x = NEW(ty)) == (ty *)0)
#define NEWVEC(ty, n)		((ty *)MALLOC((n)*sizeof(ty)))
#define NEWCLRVEC(ty, n)	((ty *)CALLOC((n), sizeof(ty)))

/* function inlining */
#if defined(NO_INLINE)
#  define STATIC_INLINE		static
#else
#  define STATIC_INLINE		static inline
#endif

#ifdef SIXTYFOUR_BIT_WORDS
#  define FILL_WORD		0xdeadbeefdeadbeef
#else
#  define FILL_WORD		0xdeadbeef
#endif

/* forward declarations of runtime system data structures */
typedef struct struct_opts Options_t;
typedef struct struct_vproc VProc_t;
typedef struct struct_chunk MemChunk_t;
typedef struct struct_logbuf LogBuffer_t;

/*! \brief a location specifies a hardware thread, core, and node in the system
 *
 * The location is encoded as node:core:thread.
 */
typedef uint32_t Location_t;

/*! \brief time values are numbers of nanoseconds */
typedef uint64_t Time_t;

STATIC_INLINE bool ValueIsBoxed (Value_t v) { return (((Addr_t)v & 0x3) == 0); }

/* type conversions  */
STATIC_INLINE Addr_t ValueToAddr (Value_t v) { return (Addr_t)v; }
STATIC_INLINE void *ValueToPtr (Value_t v) { return (void *)(Addr_t)v; }
STATIC_INLINE Word_t ValueToWord (Value_t v) { return ((Word_t)v) >> 1; }
STATIC_INLINE Value_t AddrToValue (Addr_t v) { return (Value_t)v; }
STATIC_INLINE Value_t PtrToValue (void *v) { return (Value_t)(Addr_t)v; }
STATIC_INLINE Value_t WordToValue (Word_t v) { return (Value_t)((v << 1)+1); }

extern void Die (const char *, ...)
	__attribute__ ((noreturn, format(printf, 1, 2)));
extern void Error (const char *, ...)
	__attribute__ ((format(printf, 1, 2)));
extern void Warning (const char *, ...)
	__attribute__ ((format(printf, 1, 2)));
extern void Say (const char *fmt, ...)
	__attribute__ ((format(printf, 1, 2)));

extern Value_t ApplyFun (VProc_t *vp, Value_t f, Value_t arg);
extern void RunManticore (VProc_t *vp, Addr_t codeP, Value_t arg, Value_t envP);

/* debugging support */
#ifndef NDEBUG
extern bool	DebugFlg;

extern void SayDebug (const char *fmt, ...)
	__attribute__ ((format(printf, 1, 2)));
#endif


#endif /* _MANTICORE_RT_H_ */

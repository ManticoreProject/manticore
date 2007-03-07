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
#include <stdlib.h>
/* #include "machine/manticore-regs.h" */
#include "header-bits.h"

/******************** Default sizes ********************/

#define ONE_K			1024
#define ONE_MEG			(ONE_K*ONE_K)

#ifndef MEM_BLOCK_SZB
#  define MEM_BLOCK_SZB		(256*ONE_K)
#endif
#ifndef VP_HEAP_SZB
#  define VP_HEAP_SZB		ONE_MEG
#endif


/******************** Utility definitions ********************/

typedef unsigned char Byte_t;
typedef unsigned short UInt16_t;
typedef short Int16_t;
typedef int Int32_t;
typedef unsigned int UInt32_t;
#if (SIZEOF_LONG == 8)
typedef long Int64_t;
typedef unsigned long UInt64_t;
#elif (SIZEOF_LONG_LONG == 8)
typedef long long Int64_t;
typedef unsigned long long UInt64_t;
#endif

#define MAX_WORD		((Word_t)(~0))
#define WORD_SZB		SIZEOF_CHAR_P
#ifdef SIXTYFOUR_BIT_WORDS
#  define WORD_LOGSZB		3
#else
#  define WORD_LOGSZB		2
#endif
#define BYTES_TO_WORDS(N)	(((N) + (WORD_SZB-1)) >> WORD_LOGSZB)

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

/* type conversions  */
STATIC_INLINE Addr_t ValueToAddr (Value_t v) { return (Addr_t)v; }
STATIC_INLINE Word_t *ValueToPtr (Value_t v) { return (Word_t *)(Addr_t)v; }
STATIC_INLINE Word_t ValueToWord (Value_t v) { return ((Word_t)v) >> 1; }
STATIC_INLINE Value_t AddrToValue (Addr_t v) { return (Value_t)v; }
STATIC_INLINE Value_t PtrToValue (Word_t *v) { return (Value_t)(Addr_t)v; }
STATIC_INLINE Value_t WordToValue (Word_t v) { return (Value_t)((v << 1)+1); }

extern void Die (const char *, ...)
	__attribute__ ((noreturn, format(printf, 1, 2)));
extern void Error (const char *, ...)
	__attribute__ ((format(printf, 1, 2)));
extern void Warning (const char *, ...)
	__attribute__ ((format(printf, 1, 2)));

#endif /* _MANTICORE_RT_H_ */

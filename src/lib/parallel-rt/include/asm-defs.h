/* asm-defs.h
 *
 * COPYRIGHT (c) 2007 The Manticore Project (manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Definitions for assembler files.
 */

#ifndef _ASM_DEFS_H_
#define _ASM_DEFS_H_

#ifndef NOT_C_SOURCE
#  define NOT_C_SOURCE
#endif
#include "manticore-config.h"

#define CONCAT(A,B)	A##B

#if defined(GLOBALS_HAVE_UNDERSCORE)
#  define _GSYM(ID)	CONCAT(_,ID)
#else
#  define _GSYM(ID)	ID
#endif

#endif /* _ASM_DEFS_H_ */


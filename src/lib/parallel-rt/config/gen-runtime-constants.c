/* gen-runtime-constants.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate constant values that the runtime systems shares with the 
 * code generator.
 */

#include <stdlib.h>
#include "manticore-rt.h"
#include <stdio.h>
#include "vproc.h"
#include "value.h"
#include "request-codes.h"
#include "../vproc/scheduler.h"

/* FIXME: move this somewhere else */
/* Stack frame layout */
#define SPILL_SZB	2048	/* for register spilling */
#define SAVE_AREA	(5*8)	/* for callee saves %rbx, %r12-%r15 */
#define PAD_SZB		8	/* pad so that frame size (plus saved PC and FP */
				/* is a multiple of 16 bytes */
#define FRAME_SZB	(SPILL_SZB+SAVE_AREA+PAD_SZB)


#define PR_OFFSET(obj, symb, lab)	\
	printf("\tval " #symb " = %d\n", (int)((Addr_t)&(obj.lab) - (Addr_t)&obj))
#define PR_DEFINE(symb, val)			\
	printf("\tval " #symb " = IntInf.fromInt %d\n", val)

int main () {
  VProc_t		vp;
  SchedActStkItem_t	actcons;

  printf ("structure RuntimeConstants = struct\n");

  printf ("\n(* word size and alignment *)\n");
  printf ("\tval wordSzB = 0w%d\n", sizeof (Word_t));
  printf ("\tval wordAlignB = 0w%d\n", sizeof (Word_t));
  printf ("\tval boolSzB = 0w%d\n", sizeof (Word_t));
  printf ("\tval extendedAlignB = 0w%d\n", sizeof (double));

  printf ("\n(* stack size and heap size info *)\n");
  printf ("\tval spillAreaSzB = 0w%d\n", FRAME_SZB);
  printf ("\tval maxObjectSzB = 0w%d\n", ((sizeof (Word_t)*8)-MIXED_TAG_BITS)*sizeof(Word_t));

  printf ("\n(* offsets into the VProc_t structure *)\n");
  PR_OFFSET(vp, inManticore, inManticore);
  PR_OFFSET(vp, atomic, atomic);
  PR_OFFSET(vp, sigPending, sigPending);
  PR_OFFSET(vp, allocPtr, allocPtr);
  PR_OFFSET(vp, limitPtr, limitPtr);
  PR_OFFSET(vp, stdArg, stdArg);
  PR_OFFSET(vp, stdPtr, stdEnvPtr);
  PR_OFFSET(vp, stdCont, stdCont);
  PR_OFFSET(vp, stdExnCont, stdExnCont);
  PR_OFFSET(vp, actionStk, actionStk);  

  printf("\n(* mask to get address of VProc from alloc pointer *)\n");
  printf("\tval vpMask = Word64.notb (Word64.-(0w%d,0w1))\n", VP_HEAP_SZB);

  printf("\n(* common Manticore unboxed values *)\n");
  PR_DEFINE(falseRep, M_FALSE);
  PR_DEFINE(trueRep,  M_TRUE);
  PR_DEFINE(unitRep,  M_UNIT);
  PR_DEFINE(nilRep,   M_NIL);
  
  printf ("end (* RuntimeConstants *)\n");

}

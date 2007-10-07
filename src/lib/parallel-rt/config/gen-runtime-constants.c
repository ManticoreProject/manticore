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
#include <string.h>
#include "vproc.h"
#include "value.h"
#include "request-codes.h"
#include "../vproc/scheduler.h"

extern uint32_t CRC32 (void *buf, int nBytes);

#define PR_OFFSET(obj, xxx, lab)						\
	do {									\
	    uint32_t _offset = (int)((Addr_t)&(obj.lab) - (Addr_t)&obj);	\
	    strncpy((char *)(buf+len), #lab, sizeof(#lab));			\
	    len += sizeof(#lab);						\
	    buf[len++] = ((_offset >> 8) & 0xff);				\
	    buf[len++] = _offset & 0xff;					\
	    printf("    val " #lab " : IntInf.int = %d\n", _offset);		\
	} while (0)

#define PR_DEFINE(symb, val)			\
	printf("    val " #symb " : IntInf.int = %d\n", val)

int main ()
{
    VProc_t		vp;
    SchedActStkItem_t	actcons;
    unsigned char		buf[1024];
    int			len = 0;

    printf ("structure RuntimeConstants : RUNTIME_CONSTANTS =\n");
    printf ("  struct\n");

    printf ("\n  (* word size and alignment *)\n");
    printf ("    val wordSzB : IntInf.int = %d\n", sizeof (Word_t));
    printf ("    val wordAlignB : IntInf.int = %d\n", sizeof (Word_t));
    printf ("    val boolSzB : IntInf.int = %d\n", sizeof (Word_t));
    printf ("    val extendedAlignB : IntInf.int = %d\n", sizeof (double));

    printf ("\n  (* stack size and heap size info *)\n");
    printf ("    val spillAreaSzB : IntInf.int = %d\n", FRAME_SZB);
    printf ("    val spillAreaOffB : IntInf.int = %d\n", SAVE_AREA+PAD_SZB);
    printf ("    val maxObjectSzB : IntInf.int = %d\n", ((sizeof (Word_t)*8)-MIXED_TAG_BITS)*sizeof(Word_t));

    printf ("\n  (* offsets into the VProc_t structure *)\n");
#include "vproc-offsets-ins.c"

    printf ("\n  (* mask to get address of VProc from alloc pointer *)\n");
    printf ("    val vpMask : IntInf.int = %#0lx\n", ~((Addr_t)VP_HEAP_SZB-1));

    printf ("\n  (* magic number *)\n");
    printf ("    val magic : IntInf.int = %#0x\n", CRC32(buf, len));

    printf ("\n  end (* RuntimeConstants *)\n");

}

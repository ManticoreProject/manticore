/* gen-runtime-constants.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate offsets into the runtime-system data structures that are used
 * in the scheduler HLOps.
 */

#include <stdlib.h>
#include <stdbool.h>
#include "manticore-rt.h"
#include <stdio.h>
#include <string.h>
#include "vproc.h"
#include "value.h"
#include "../vproc/scheduler.h"

extern uint32_t CRC32 (void *buf, int nBytes);

#define PR_OFFSET(obj, xxx, lab, local)						\
	do {									\
	    uint32_t _offset = (int)((Addr_t)&(obj.lab) - (Addr_t)&obj);	\
	    strncpy((char *)(buf+len), #lab, sizeof(#lab));			\
	    len += sizeof(#lab);						\
	    buf[len++] = ((_offset >> 8) & 0xff);				\
	    buf[len++] = _offset & 0xff;					\
	    printf("#define " #xxx " %d\n", _offset);				\
	} while (0)

#define PR_DEFINE(symb, val)			\
	printf("    val " #symb " : IntInf.int = %d\n", val)

int main ()
{
    VProc_t		vp;
    SchedActStkItem_t	actcons;
    unsigned char		buf[1024];
    int			len = 0;

    printf ("/* runtime-offsets.def\n");
    printf (" *\n");
    printf (" * WARNING: this file is generated; do not edit!!!\n");
    printf (" */\n");
    printf ("\n#ifndef _RUNTIME_OFFSETS_DEF_\n");
    printf ("\n#define _RUNTIME_OFFSETS_DEF_\n");

#include "vproc-offsets-ins.c"

    printf ("\n/* magic number */\n");
    printf ("#define MAGIC %#0x\n", CRC32(buf, len));

    printf ("#endif /* !_RUNTIME_OFFSETS_DEF_ */\n");

    return 0;
}

#ifndef _ASM_OFFSETS_H_
#define _ASM_OFFSETS_H_

/* offsets into the VProc_t structure */
#define IN_MANTICORE 0
#define ATOMIC 4
#define SIG_PENDING 8
#define ALLOC_PTR 12
#define LIMIT_PTR 16

/* mask to get address of VProc from alloc pointer */
#define VP_MASK 0xfff00000

/* request codes for when Manticore returns to C */
#define REQ_RequestGC 0
#define REQ_Return 1
#define REQ_RaiseExn 2

#endif

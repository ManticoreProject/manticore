/* registers.h
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Manticore runtime conventions on the AMD64:
 *
 */

#ifndef _REGISTERS_H_
#define _REGISTERS_H_

#define STD_ARG_REG         %rax          /* standard arg */
#define STD_EP_REG          %rdi          /* standard environment pointer */
#define STD_CONT_REG        %r8           /* standard return continuation */
#define STD_EXH_REG         %r9           /* standard exception handler */
#define ALLOC_PTR_REG       %rsi          /* allocation pointer */
#define LIMIT_PTR_REG       %r11          /* limit pointer */

#endif /* !_REGISTERS_H_ */

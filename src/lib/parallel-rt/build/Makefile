# Makefile
#
#

CC =		gcc
CFLAGS =	-std=gnu99 -g -O2 -m64
LDFLAGS =	
CPPFLAGS =	-I../include
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBS		= -lpthreads

TARGET		= libmanticore.a

VPATH		= ../include:../gc:../vproc:../misc:../misc/machine:../config

GC_SRCS 	= minor-gc.c \
		  major-gc.c \
		  global-gc.c \
		  unix-memory.c \
		  options.c

VPROC_SRCS	= vproc.c

C_SRCS		= $(GC_SRCS) $(VPROC_SRCS)
C_OBJS		= $(patsubst %.c,%.o,$(C_SRCS))

ASM_SRCS	= asm-glue.S
ASM_OBJS	= $(patsubst %.S,%.o,$(ASM_SRCS))

OBJS		= $(C_OBJS) $(ASM_OBJS)

$(TARGET):	$(OBJS)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJS)
	$(RANLIB) $(TARGET)


$(ASM_OBJS): %.o : %.S asm-offsets.h
	$(CC) -c -m64 -I. -DNOT_C_SOURCE $(CPPFLAGS) -o $@ $<

asm-offsets.h:		gen-asm-offsets
	gen-asm-offsets > asm-offsets.h

gen-asm-offsets:	gen-asm-offsets.c manticore-rt.h vproc.h request-codes.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -o gen-asm-offsets $<

.PHONY:		clean

clean:
	rm -rf $(OBJS) $(TARGET) gen-asm-offsets asm-offsets.h


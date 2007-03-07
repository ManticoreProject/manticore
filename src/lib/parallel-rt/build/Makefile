# Makefile
#
#

CC =		gcc
CFLAGS =	-std=gnu99 -g -O2
LDFLAGS =	
CPPFLAGS =	-I../include
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBS		= -lpthreads

TARGET		= libmanticore.a

VPATH		= ../include:../gc:../vproc:../misc:../config

GC_SRCS 	= minor-gc.c \
		  major-gc.c \
		  global-gc.c \
		  unix-memory.c \
		  options.c

VPROC_SRCS	= vproc.c

C_SRCS		= $(GC_SRCS) $(VPROC_SRCS)
C_OBJS		= $(patsubst %.c,%.o,$(C_SRCS))

$(TARGET):	$(C_OBJS)
	$(AR) $(ARFLAGS) $(TARGET) $(C_OBJS)
	$(RANLIB) $(TARGET)

asm-offsets.h:		gen-asm-offsets
	gen-asm-offsets > asm-offsets.h

gen-asm-offsets:	gen-asm-offsets.c manticore-rt.h vproc.h request-codes.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -o gen-asm-offsets $<

.PHONY:		clean

clean:
	rm -rf $(C_OBJS) $(TARGET) gen-asm-offsets


#ifndef _MANTICORE_TYPES_H
#define _MANTICORE_TYPES_H

enum { FALSE=0, TRUE };
typedef int Bool_t;

typedef unsigned long Word_t;
typedef unsigned int uint_t;

#define NEW(ty)     (ty*)malloc(sizeof(ty))
#define FREE(x)     (free (x))

#endif

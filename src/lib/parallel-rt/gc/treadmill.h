/* treadmill.h
 *
 * COPYRIGHT (c) 2018 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Data structures and utilities for working with a non-moving large-object
 * area that is managed using Baker's Treadmill.
 *
 * Throughout the comments below, we use LO to mean "large object"
 */

#ifndef _TREADMILL_H_
#define _TREADMILL_H_

#include <inttypes.h>
#include <stdlib.h>


// NOTE: it is likely that we will have handwritten ASM that accesses
// the treadmill, so if you need to align the fields use this:
#define ALIGN_8  __attribute__ ((aligned (8)))

#define ALWAYS_INLINE inline __attribute__((always_inline))

////////////////////////////////////////////////
/////////////     TYPES     ////////////////////

// all objects are assumed to be the same size.
// invariants:
// 1. if lo1->right == lo2, then lo2->left == lo1
// 2. if lo1->left == lo3, then lo3->right == lo1
//
// If the treadmill is drawn on the plane as a circle, the
// direction "left" and "right" are to be interpreted from
// the perspective of someone observing an element in the chain
// from the _outside_ of the chain.
typedef struct Self {
  struct Self* left;
  struct Self* right;
  uint8_t contents[];
} LargeObject_t;


// invariant:
// ... { BLACK } scan { GREY } top { WHITE } bottom { FREE } free ...
typedef struct {
  LargeObject_t* scan;
  LargeObject_t* top;
  LargeObject_t* bottom;
  LargeObject_t* free;
  size_t size;      // size of large objects in this treadmill
} Treadmill_t;

////////////////////////////////////////////////////
/////////////     UTILITIES     ////////////////////

// insert val to the right of tgt.
ALWAYS_INLINE void lo_ins_RIGHTof (LargeObject_t* tgt, LargeObject_t* val) {
  LargeObject_t* oldRight = tgt->right;
  // make tgt and val hold hands.
  tgt->right = val;
  val->left = tgt;
  // make val and oldRight hold hands.
  val->right = oldRight;
  oldRight->left = val;
}

// insert val to the left of tgt.
ALWAYS_INLINE void lo_ins_LEFTof (LargeObject_t* tgt, LargeObject_t* val) {
  LargeObject_t* oldLeft = tgt->left;
  // make tgt and val hold hands.
  tgt->left = val;
  val->right = tgt;
  // make val and oldLeft hold hands.
  val->left = oldLeft;
  oldLeft->right = val;
}

// "unsnap" the object from the treadmill
ALWAYS_INLINE void lo_remove(LargeObject_t* val) {
  LargeObject_t* leftObj = val->left;
  LargeObject_t* rightObj = val->right;

  leftObj->right = rightObj;
  rightObj->left = leftObj;
}

// allocates a new, uninitialized LargeObject of the given size.
ALWAYS_INLINE LargeObject_t* lo_create_new(size_t sz) {
  // TODO alignment?, don't use malloc.
  LargeObject_t* mem = (LargeObject_t*) malloc(sizeof(LargeObject_t) + sz);
  return mem;
}


////////////////////////////////////////////////////
/////////////     OPERATIONS     ///////////////////


// TODO: tm is a candidate for marking it `restrict`
uint8_t* tm_alloc(Treadmill_t* tm) {
  LargeObject_t* curFreeLO = tm->free;

  if (curFreeLO == tm->bottom) {
    // we're out of free LO's, so we obtain a new one.
    LargeObject_t* mem = lo_create_new(tm->size);
    lo_ins_RIGHTof(curFreeLO, mem);
    return mem->contents;
  }

  // move free pointer to the left
  tm->free = curFreeLO->left;
  return curFreeLO->contents;
}

// initializes a new treadmill that manages objects of the given size.
// FIXME: I feel like we need at least two free areas to get going?
void tm_init(Treadmill_t* tm, size_t size) {
  LargeObject_t* firstFree = lo_create_new(size);

  firstFree->left = firstFree;
  firstFree->right = firstFree;

  tm->free = firstFree;
  tm->top = firstFree;
  tm->bottom = firstFree;
  tm->scan = firstFree;
  tm->size = size;
}


////////////////////////////////////////////////////
//////////     COLLECTION PROCEDURES     ///////////


// FIXME: in flux right now:
void tm_start_gc(Treadmill_t* tm) {
  // (1) swap semi-spaces and setup for scanning.

  LargeObject_t* oldTop = tm->top;
  tm->top = tm->bottom;
  tm->bottom = oldTop;



  // (2) mark roots grey by moving them into the tospace as a grey
  // object. This initializes the "scan queue" which is processed
  // by moving to the right later on.

  LargeObject_t* greyStart = tm->top;
  // for i in roots:
  //   lo_remove(i)
  //   lo_ins_LEFTof(greyStart, i);
  //   scanTop = i;
  tm->scan = greyStart;

  // (3) scan treadmill's tospace
  while (tm->scan != tm->top) {
    uint8_t* contents = tm->scan->contents;

    // scan contents for more pointers,
    // if any treadmill pointers are encountered,
    // use tm_forward_* to

    // move to the next object
    tm->scan = tm->scan->right;
  }
    // do stuff

  // (4) set free pointer
}

// forward this object to the tospace to be scanned in DFS-order
void tm_forward_dfs(Treadmill_t* tm, LargeObject_t *obj) {
  lo_remove(obj);
  lo_ins_RIGHTof(tm->scan, obj);
}

// forward this object to the tospace to be scanned in BFS-order
void tm_forward_bfs(Treadmill_t* tm, LargeObject_t *obj) {
  lo_remove(obj);
  lo_ins_LEFTof(tm->top, obj);
}




#endif /* _TREADMILL_H_ */

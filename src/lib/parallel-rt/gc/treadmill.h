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
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <stddef.h>


// NOTE: it is likely that we will have handwritten ASM that accesses
// the treadmill, so if you need to align the fields use this:
#define ALIGN_8  __attribute__ ((aligned (8)))

#define ALWAYS_INLINE inline __attribute__((always_inline))

////////////////////////////////////////////////
/////////////     TYPES     ////////////////////

// We need to maintain an efficient means of testing whether an object
// is in the tospace or not, which flips often. We use flag terminology
// instead of colors to avoid confusion.
typedef bool Flag_t;

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
  Flag_t flag;
  uint8_t contents[];
} LargeObject_t;


// invariant:  "->" is direction of movement, "|" means it's a limit
//
//                   ->                |              |             <-
// ... { BLACK } .. scan .. GREY .. { top,  WHITE, bottom } { TAN, free } ...
typedef struct {
  LargeObject_t* scan;
  LargeObject_t* top;
  LargeObject_t* bottom;
  LargeObject_t* free;
  size_t size;      // size of each large object
  size_t elms;      // number of large objects
  Flag_t fromSpaceFlag;
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

///////////
// TODO: tm is a candidate for marking it `restrict`
uint8_t* tm_alloc(Treadmill_t* tm) {
  LargeObject_t* curFreeLO = tm->free;

  if (curFreeLO == tm->bottom) {
    // we're out of free LO's, so we grow the heap.

    // allocate a LO in tospace and mark it Black immediately.
    LargeObject_t* mem = lo_create_new(tm->size);
    mem->flag = !(tm->fromSpaceFlag);
    lo_ins_RIGHTof(curFreeLO, mem);

    // QUESTION do we really need a corresponding LO entry
    // in the fromspace to keep the sides even when flipping?

    // LargeObject_t* mirror = lo_create_new(tm->size);
    // mirror->flag = tm->fromSpaceFlag;
    // lo_ins_LEFTof(tm->bottom, mirror);

    tm->elms += 1;

    return mem->contents;
  }

  // the free list should contain Tan objects that are logically in tospace.
  assert( (curFreeLO->flag == !(tm->fromSpaceFlag))
         && "invariant violation: all new allocations are made in the tospace"
       );

  // move free pointer to the left
  tm->free = curFreeLO->left;
  return curFreeLO->contents;
}

///////////
// initializes a new treadmill that manages objects of the given size.
void tm_init(Treadmill_t* tm, size_t size) {
  const size_t numLOs = 16; // must be an EVEN number

  // setup other metadata of the treadmill
  tm->size = size;
  tm->fromSpaceFlag = true; // arbitrary starting value.
  tm->elms = numLOs;

  const Flag_t fromSpFlag = tm->fromSpaceFlag;
  const Flag_t toSpFlag = !fromSpFlag;
  LargeObject_t* first = lo_create_new(size);

  {
    // construct the treadmill's structure
    LargeObject_t* cur = first;
    LargeObject_t* next = NULL;
    for (size_t i = 0; i < numLOs-1; i++) {
      next = lo_create_new(size);
      cur->right = next;
      next->left = cur;
      cur = next;
    }
    // complete the cycle.
    cur->right = first;
    first->left = cur;
  }

  // partition the treadmill.
  tm->top = first;
  tm->scan = tm->top; // no greys
  tm->free = tm->scan->left; // no blacks, aka, objects currently allocated in tospace

  // setup the bounds of the semi-spaces. the fromspace spans
  // top -> bottom, inclusive
  LargeObject_t* cur = tm->top;
  for (size_t i = 0; i < (numLOs / 2); i++) {
    cur->flag = fromSpFlag;
    cur = cur->right;
  }

  // at this point, cur points to a tospace node that is unvisited,
  // so we look left
  tm->bottom = cur->left;

  // mark flags for the tospace, which spans bottom -> top, exclusive.
  // aka, everything outside of top -> bottom.
  while (cur != tm->top) {
    cur->flag = toSpFlag;
    cur = cur->right;
  }

}


////////////////////////////////////////////////////
//////////     COLLECTION PROCEDURES     ///////////


// forward this object to the tospace to be scanned in breadth-first order.
// this marks the object as Grey.
ALWAYS_INLINE void tm_forward_bfs(Treadmill_t* tm, LargeObject_t* obj) {
  const Flag_t toSpace = !(tm->fromSpaceFlag);

  if (obj->flag == toSpace) {
    // fprintf(stderr, "Already in tospace\n");
    return;
  }

  // since obj is in the from-space, to remove it, we need to check
  // if its one of the ends of that subsequence and adjust the head/tail
  // accordingly

  if (tm->top == obj)
    tm->top = obj->right;
  else if (tm->bottom == obj)
    tm->bottom = obj->left;

  lo_remove(obj);
  obj->flag = toSpace; // mark tospace

  lo_ins_LEFTof(tm->top, obj);

  if (tm->scan == tm->top) {
    // need to move scan ptr over since this is the first grey object.
    tm->scan = tm->top->left;
  }
}


///////////
// TODO: mark tm as restrict
void tm_start_gc(Treadmill_t* tm, LargeObject_t** roots) {
  // To remove this assertion, we would need to clear out the
  // grey nodes by scanning all of them immediately before
  // continuing, because we can't flip spaces with grey nodes.
  assert( (tm->scan == tm->top)
          && "incremental collection is not supported."
        );

  //////////////////
  // Precondition: ALL objects in the tospace have been scanned. That is,
  // there are no Grey objects in tospace, only Black or Tan.

  // (1) swap semi-spaces
  //   ____________________________
  //  v                           |
  // [O] [O] [X] [X] [X] [X] [O] [O]
  //          ^           ^
  //         top ----> bottom
  //
  //        ~~ FLIP ~~>
  //
  //   ____________________________
  //  v                           |
  // [O] [O] [X] [X] [X] [X] [O] [O]
  //      ^                   ^
  // --> bottom              top --

  LargeObject_t* oldTop = tm->top;
  LargeObject_t* oldBottom = tm->bottom;

  if (oldBottom == tm->free) {
    // then we can't do an even flip, since the right-side of
    // bottom is allocated. instead we just swap top/bottom.
    tm->top = oldBottom;
    tm->bottom = oldTop;
  } else {
    tm->top = oldBottom->right;
    tm->bottom = oldTop->left;
  }

  // swap the meaning of the flags.
  tm->fromSpaceFlag = !(tm->fromSpaceFlag);

  // setup other pointers
  tm->scan = tm->top;
  tm->free = tm->top->left;


  // (3) make roots Grey by moving them into the tospace as a Grey
  // object.
  while (*roots != NULL) {
    tm_forward_bfs(tm, *roots);
    roots++;
  }

  // (4) scan treadmill's tospace
  size_t numLive = 0;
  while (tm->scan != tm->top) {
    numLive++;
    // uint8_t* contents = tm->scan->contents;

    // TODO scan "contents" for more pointers,
    // if any treadmill pointers are encountered,
    // use tm_forward_* to put it on the scan queue.

    // move to the next object
    tm->scan = tm->scan->right;
  }

  // (5) split the remaining free space in half, as from and to space.
  /* XXX below is an attempt to perform the rebalancing that doesn't work.
  const size_t tot = tm->elms;
  size_t leftover = tot - numLive;
  size_t diff = (tot / 2) - (leftover / 2);

  fprintf(stderr, "leftover = %zd, diff = %zd\n", leftover, diff);

  bool moveRight = !(tm->fromSpaceFlag); // YUCK
  while (diff > 0) {
    if (moveRight)
      tm->bottom = tm->bottom->right;
    else
      tm->bottom = tm->bottom->left;
    diff--;
  }
  */

  return;
} // end of tm_start_gc


////////////////////////////////////////////////////
//////////     DEBUGGING PROCEDURES      ///////////

void tm_show(Treadmill_t* tm) {
  size_t numWhite = 0;
  size_t numGrey = 0;
  size_t numBlack = 0;
  size_t numTan = 0;
  size_t numFromSpace = 0;

  LargeObject_t* cur = tm->top;
  char color = 'w';
  LargeObject_t* colorEndL = tm->bottom;
  LargeObject_t* colorEndC = NULL;
  do {
    numFromSpace = cur->flag == tm->fromSpaceFlag
                    ? numFromSpace + 1
                    : numFromSpace;

    // it turns out to be rather complicated to figure out what
    // particular color items are moving left-to-right.
    while (cur->left == colorEndL || cur == colorEndC) {
      switch(color) {
        case 'w':
          color = 't';
          colorEndL = tm->free;
          colorEndC = NULL;
          break;

        case 't':
          color = 'b';
          colorEndL = NULL;
          colorEndC = tm->scan;
          break;

        case 'b':
          color = 'g';
          colorEndL = NULL;
          colorEndC = tm->top;
          break;

        default:
          fprintf(stderr, "error in color transition machine.\n");
          exit(1);
      };
    }

    // gather stats
    switch (color) {
      case 'w': numWhite++; break;
      case 'g': numGrey++; break;
      case 'b': numBlack++; break;
      case 't': numTan++; break;
      default: fprintf(stderr, "impossible color"); exit(1);
    };

    // determine what to print, with some built-in error checking
    // so that what is printed is not misleading.
    if ( cur != tm->top
      && cur != tm->bottom
      && cur != tm->free
      && cur != tm->scan
      ) {
        fprintf(stderr, "%c ", color);

    } else if (cur == tm->top) {
        assert(color == 'w');
        fprintf(stderr, "T ");

    } else if (cur == tm->bottom) {
        assert(color == 'w');
        fprintf(stderr, "B\n");

    } else if (cur == tm->free) {
        assert(color == 't' || color == 'w');
        fprintf(stderr, "F ");

    } else if (cur == tm->scan) {
        assert(color == 'g' || color == 'w');
        // while scan can point to white too, it will only be the case
        // if scan == top == cur, which is handled earlier.
        fprintf(stderr, "S ");

    } else {
      fprintf(stderr, "[ERROR] ");
    }

    cur = cur->right; // advance
  } while (cur != tm->top);


  fprintf(stderr, "\n");

  fprintf(stderr, "numFromSpace = %zd \n\n", numFromSpace);

}

#endif /* _TREADMILL_H_ */

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


/////////////////
//  Build options
////////////////

// It is still unclear whether rebalancing is useful or not, so its optional.
//#define TM_REBALANCE

// For debugging, make sure you're also not defining NDEBUG
// #define TM_DEBUG_GC

// Gathers some additional stats that will be included in tm_show
// #define TM_STATS

////////////////////////////////


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
  Flag_t fromSpaceFlag;

#ifdef TM_REBALANCE
  size_t fromSpaceElms; // number of LO's in the from-space.
  size_t toSpaceElms; // number of LO's in the to-space.
#endif

#ifdef TM_STATS
  // statistics gathering
  size_t allocHits;     // number of allocs that hit the free list
  size_t allocMisses;
#endif

} Treadmill_t;

////////////////////////////////////////////////////
/////////////     UTILITIES     ////////////////////

void tm_show(Treadmill_t* tm);

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
    assert(curFreeLO->flag == tm->fromSpaceFlag);

    // allocate a LO in tospace and mark it Black immediately.
    LargeObject_t* mem = lo_create_new(tm->size);
    mem->flag = !(tm->fromSpaceFlag);
    lo_ins_RIGHTof(curFreeLO, mem);

    #ifdef TM_REBALANCE
      tm->toSpaceElms += 1;
    #endif

    #ifdef TM_STATS
      tm->allocMisses += 1;
    #endif

    return mem->contents;
  }

  // the free list should contain Tan objects that are logically in tospace.
  assert( (curFreeLO->flag == !(tm->fromSpaceFlag))
         && "invariant violation: all new allocations are made in the tospace"
       );

  #ifdef TM_STATS
   tm->allocHits += 1;
  #endif

  // move free pointer to the left
  tm->free = curFreeLO->left;
  return curFreeLO->contents;
}

///////////
// initializes a new treadmill that manages objects of the given size.
void tm_init(Treadmill_t* tm, size_t size) {
  const size_t numLOs = 16; // must be an EVEN number
  const size_t fromSpaceElms = numLOs / 2;

  // setup other metadata of the treadmill
  tm->size = size;
  tm->fromSpaceFlag = true; // arbitrary starting value.

#ifdef TM_REBALANCE
  tm->fromSpaceElms = fromSpaceElms;
#endif

  #ifdef TM_STATS
    tm->allocHits = 0;
    tm->allocMisses = 0;
  #endif

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
  for (size_t i = 0; i < fromSpaceElms; i++) {
    cur->flag = fromSpFlag;
    cur = cur->right;
  }

  // at this point, cur points to a tospace node that is unvisited,
  // so we look left
  tm->bottom = cur->left;

  // mark flags for the tospace, which spans bottom -> top, exclusive.
  // aka, everything outside of top -> bottom.
  size_t toSpaceElms = 0;
  while (cur != tm->top) {
    cur->flag = toSpFlag;
    cur = cur->right;
    toSpaceElms++;
  }

#ifdef TM_REBALANCE
  tm->toSpaceElms = toSpaceElms;
#endif

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

  // since obj is in the from-space, to remove it, we first need to check
  // if its one of the ends of that subsequence and adjust the head/tail
  // accordingly

  if (tm->top == obj) {
    tm->top = obj->right;

    // NOTE: If the object being forwarded was the last element
    // in the from-space, then tm->top == obj == tm-> bottom,
    // which is equivalent to top->right->flag == tospace.
    //
    // Thus the element to the right of top is actually in
    // the free list, so we need to correctly move it to from-space.
    //
    // Since the old, non-empty from-space became the free list
    // in the current to-space, we can safely steal one of those LO's
    // so that from-space is not empty, which is our invariant.
    if (tm->top->flag == toSpace) {
      tm->top->flag = !toSpace;
      tm->bottom = tm->top; // don't forget to set bottom here too!

      #ifdef TM_REBALANCE
        tm->fromSpaceElms +=  1;
        tm->toSpaceElms   += -1;
      #endif
    }

  } else if (tm->bottom == obj) {
    tm->bottom = obj->left;
    assert(tm->bottom->flag == !toSpace);
  }

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

  tm->top = oldBottom->right;
  tm->bottom = oldTop->left;

  #ifdef TM_REBALANCE
    // update element counts
    size_t tmp = tm->fromSpaceElms;
    tm->fromSpaceElms = tm->toSpaceElms;
    tm->toSpaceElms = tmp;
  #endif

  // swap the meaning of the flags.
  tm->fromSpaceFlag = !(tm->fromSpaceFlag);
  const Flag_t toSpFlag = !(tm->fromSpaceFlag);
  const Flag_t frmSpFlag = tm->fromSpaceFlag;

  // setup other pointers
  tm->scan = tm->top;
  tm->free = tm->top->left;

  assert(tm->free->flag == toSpFlag);
  assert(tm->scan->flag == frmSpFlag);

#ifdef TM_DEBUG_GC
  fprintf(stderr, "\tflipped:\n");
  tm_show(tm);
#endif


  // (2) make roots Grey by moving them into the tospace as a Grey
  // object.
  while (*roots != NULL) {
    tm_forward_bfs(tm, *roots);
    roots++;
  }

#ifdef TM_DEBUG_GC
  fprintf(stderr, "\tafter roots forwarded:\n");
  tm_show(tm);
#endif


  // (3) scan treadmill's tospace
  size_t numLive = 0;
  while (tm->scan != tm->top) {
    numLive++;
    assert(tm->scan->flag == toSpFlag);
    // uint8_t* contents = tm->scan->contents;

    // TODO scan "contents" for more pointers,
    // if any treadmill pointers are encountered,
    // use tm_forward_* to put it on the scan queue.

    // move to the next object
    tm->scan = tm->scan->right;
  }

#ifdef TM_DEBUG_GC
  fprintf(stderr, "after scanned:\n");
  tm_show(tm);
#endif // TM_DEBUG_GC


#ifdef TM_REBALANCE

  // (4) rebalance the semi-spaces.
  //     this is a O(unallocated / 2) traversal.

  size_t fromSpaceElms = tm->fromSpaceElms - numLive;
  size_t toSpaceElms = tm->toSpaceElms + numLive;
  const size_t tot = fromSpaceElms + toSpaceElms;

  // we allot half of the unused area to the free list.
  size_t freeSz = (tot - numLive) / 2;
  size_t freeListElms = toSpaceElms - numLive;
  ptrdiff_t spares = ((ptrdiff_t)(freeListElms)) - ((ptrdiff_t)(freeSz));

  assert(tm->top->flag == frmSpFlag);
  assert(tm->bottom->flag == frmSpFlag);

  // take away the excess from the free list
  for (ptrdiff_t i = spares; i > 0; i--) {
    LargeObject_t* newBot = tm->bottom->right;

    assert( newBot->flag == toSpFlag );
    newBot->flag = frmSpFlag;

    tm->bottom = newBot;

    toSpaceElms   += -1;
    fromSpaceElms +=  1;
  }

  // give from-space excess to the free list
  for (ptrdiff_t i = spares; i < 0; i++) {
    LargeObject_t* newTan = tm->bottom;

    assert( newTan->flag == frmSpFlag );
    newTan->flag = toSpFlag;

    tm->bottom = newTan->left;

    toSpaceElms   +=  1;
    fromSpaceElms += -1;
  }

  assert(tm->bottom->flag == frmSpFlag);

  tm->fromSpaceElms = fromSpaceElms;
  tm->toSpaceElms = toSpaceElms;

  fprintf(stderr, "freeListSurplus = %zd\n", spares);

#ifdef TM_DEBUG_GC
  fprintf(stderr, "after rebalancing:\n");
  tm_show(tm);
#endif // TM_DEBUG_GC

#endif // REBALANCE

  return;
} // end of tm_start_gc


////////////////////////////////////////////////////
//////////     DEBUGGING PROCEDURES      ///////////

void tm_show(Treadmill_t* tm) {
  size_t numWhite = 0;
  size_t numGrey = 0;
  size_t numBlack = 0;
  size_t numTan = 0;
  size_t numMarkedFromSpace = 0;

#ifdef TM_REBALANCE
  const size_t maxPrintWidth = 128;

  const bool tooBig =
    (    tm->fromSpaceElms > maxPrintWidth
      || tm->toSpaceElms > maxPrintWidth  );
#else
  // no count info is kept, so lets play it safe and not spam output.
  const bool tooBig = true;
#endif

  LargeObject_t* cur = tm->top;
  char color = 'w';
  LargeObject_t* colorEndL = tm->bottom;
  LargeObject_t* colorEndC = NULL;

  const Flag_t isFromSp = tm->fromSpaceFlag;
  const Flag_t isToSp = !isFromSp;
  do {
    // corruption check
    assert(cur->flag == true || cur->flag == false);

    Flag_t flag = cur->flag;
    numMarkedFromSpace = flag == isFromSp
                       ? numMarkedFromSpace + 1
                       : numMarkedFromSpace;

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

    // gather stats and check color invariants
    switch (color) {
      case 'w': numWhite++; assert(flag == isFromSp);
                break;
      case 'g': numGrey++;  assert(flag == isToSp);
                break;
      case 'b': numBlack++; assert(flag == isToSp);
                break;
      case 't': numTan++;   assert(flag == isToSp);
                break;
      default: fprintf(stderr, "impossible color"); exit(1);
    };

    // determine what to print, with some built-in error checking
    // so that what is printed is not misleading.
    if(!tooBig) {
      if ( cur != tm->top
        && cur != tm->bottom
        && cur != tm->free
        && cur != tm->scan
        ) {
          fprintf(stderr, "%c", color);

      } else if (cur == tm->top) {
          assert(color == 'w');
          fprintf(stderr, "T");

      } else if (cur == tm->bottom) {
          assert(color == 'w');
          fprintf(stderr, "B\n");

      } else if (cur == tm->free) {
          assert(color == 't' || color == 'w');
          fprintf(stderr, "F");

      } else if (cur == tm->scan) {
          assert(color == 'g' || color == 'w');
          // while scan can point to white too, it will only be the case
          // if scan == top == cur, which is handled earlier.
          fprintf(stderr, "S");

      } else {
        assert(false && "misc error");
        fprintf(stderr, " [ERROR] ");
      }
    }

    cur = cur->right; // advance
  } while (cur != tm->top);

  fprintf(stderr, "\nW%zd, T%zd, G%zd, B%zd\n", numWhite, numTan, numGrey, numBlack);

  #ifdef TM_STATS
    if (tm->allocMisses > 0) {
      double ratio = (double)(tm->allocHits) / (double)(tm->allocMisses);
      fprintf(stderr, "hit/miss ratio = %f\n", ratio);
    } else {
      fprintf(stderr, "no misses\n");
    }
  #endif

  fprintf(stderr, "\n");

}

#endif /* _TREADMILL_H_ */

(* debug.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

_primcode (
  extern void M_Print (void*);
  extern void M_PrintInt (int);
  extern void M_PrintPtr (void *, void *);
  extern void M_PrintDebug (void*);
  extern void M_PrintLong (long);
  extern void M_PrintDebugMsg (void*, void*, void*, int);
  extern void M_PrintTestingMsg (void*, void*, int);
)

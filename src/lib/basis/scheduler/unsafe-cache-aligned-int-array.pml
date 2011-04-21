(* unsafe-cache-aligned-int-array.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Cache-aligned arrays of integers.
 *
 * The purpose of this module is to provide efficient private VProc
 * arrays. In particular, we avoid false sharing between VProcs by
 * ensuring that each array is cache aligned.
 *
 *)

#define CACHE_LINE_SZB      128
#define CACHE_LINE_SZ       (CACHE_LINE_SZB div 4)

structure UnsafeCacheAlignedIntArray (* :>
    type array
    val create  : int -> array
    val sub     : array * int -> int
    val update  : array * int * int -> unit
  end *) = struct
    type array = UnsafeIntArray.array
    fun create n = UnsafeIntArray.create (2*CACHE_LINE_SZ + n)
    fun sub (a, i) = UnsafeIntArray.sub (a, i + CACHE_LINE_SZ)
    fun update (a, i, x) = UnsafeIntArray.update (a, i + CACHE_LINE_SZ, x)
end

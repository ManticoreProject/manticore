(* unsafe-cache-aligned-float-array.pml
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

structure UnsafeCacheAlignedFloatArray (* :>
    type array
    val create  : float -> array
    val sub     : array * float -> float
    val update  : array * float * float -> unit
  end *) = struct
    type array = UnsafeFloatArray.array
    fun create n = UnsafeFloatArray.create (2*CACHE_LINE_SZ + n)
    fun sub (a, i) = UnsafeFloatArray.sub (a, i + CACHE_LINE_SZ)
    fun update (a, i, x) = UnsafeFloatArray.update (a, i + CACHE_LINE_SZ, x)
end

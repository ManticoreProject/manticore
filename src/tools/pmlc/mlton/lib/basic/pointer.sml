(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Pointer: POINTER =
struct

datatype 'a t = T of 'a option ref

fun !(T r) =
   case MLtonRef.! r of
      NONE => Error.bug "Pointer.!"
    | SOME v => v

fun (T r) := v = MLtonRef.:=(r, SOME v)

fun clear(T r) = MLtonRef.:=(r, NONE)

fun copy(T r, T r') = MLtonRef.:=(r, MLtonRef.! r')

fun eq(T r, T r') = MLtonRef.equals(r, r')

fun follow(T r) = MLtonRef.! r

fun equals(p, p', equals) =
   case (follow p, follow p') of
      (NONE, NONE) => true
    | (SOME v, SOME v') => equals(v, v')
    | _ => false

fun isNull p = MLtonOption.isNone(follow p)

fun make v = T(ref v)

fun new v = make(SOME v)

fun null() = make NONE

fun swap(T p, T p') = MLtonRef.swap(p, p')

end

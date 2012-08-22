(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure InsertionSort: INSERTION_SORT =
struct

fun Int_for (start, stop, f: int -> unit): unit = let
      fun lp i = if (i < stop) then (f i; lp(i+1)) else ()
      in
	lp start
      end
fun Int_forall (start, stop, f: int -> bool): bool = let
      fun lp i = (i >= stop) orelse (f i andalso lp(i+1))
      in
	lp start
      end

open Array

(* Based on page 108 of Programming Pearls, by Bentley. *)
fun sort (a: 'a array, op <= : 'a * 'a -> bool): unit =
   let
      fun x i = sub (a, i)
      val _ =
         (*Int.for*)Int_for
         (1, Array.length a, fn i =>
          let
(*
             val _ =
                if true
                   then ()
                else
                   Assert.assert ("InsertionSort.sort: 1", fn () =>
                                  Array.isSortedRange (a, 0, i, op <=))
*)
             val t = x i
             fun sift (j: int) =
                (if true
                    then ()
                 else
                    Assert.assert
                    ("InsertionSort.sort: 2", fn () =>
(*
                     Array.isSortedRange (a, 0, j, op <=)
                     andalso Array.isSortedRange (a, j + 1, i + 1, op <=)
                     andalso*) (*Int.forall*)Int_forall (j + 1, i + 1, fn k => t <= x k))
                 ; if j > 0
                      then
                         let
                            val j' = j - 1
                            val z = x j'
                         in
                            if z <= t
                               then j
                            else (update (a, j, z)
                                  ; sift j')
                         end
                   else j)
             val _ = update (a, sift i, t)
          in ()
          end)
(*
      val _ =
         Assert.assert ("InsertionSort.sort: 3", fn () => isSorted (a, op <=))
*)
   in
      ()
   end

end

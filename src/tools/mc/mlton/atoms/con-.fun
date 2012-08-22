(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Con (S: CON_STRUCTS): CON = 
struct

fun Int_layout i = Layout.str(Int.toString i)

open S

structure C = Id (val noname = "C")
open C

structure P = PrimCons (C)
open P

val all = [cons, falsee, nill, reff, truee, bind, match]

fun stats () =
   let
      open Layout
   in
      align
      (MLtonList.map (all, fn c =>
                 seq [layout c, str " size is ",
(*
                      Int.layout (MLton.size c),
*)
		      str "??",
                      str " plist length is ",
                      (*Int.layout*)Int_layout (PropertyList.length (plist c))]))
   end
(* quell unused warning *)
val _ = stats

fun fromBool b = if b then truee else falsee

end

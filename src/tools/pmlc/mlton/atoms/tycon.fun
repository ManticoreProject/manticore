(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Tycon (S: TYCON_STRUCTS): TYCON = 
struct

fun Int_layout i = Layout.str(Int.toString i)

open S

structure Id = Id (val noname = "t")
open Id

structure P = PrimTycons (structure AdmitsEquality = AdmitsEquality
                          structure CharSize = CharSize
                          structure IntSize = IntSize
                          structure Kind = Kind
                          structure RealSize = RealSize
                          structure WordSize = WordSize
                          open Id)
open P

val setPrintName =
   Trace.trace2 ("Tycon.setPrintName", layout, (*String.layout*)Layout.str, Unit.layout)
   setPrintName

fun stats () =
   let
      open Layout
   in
      align
      (MLtonList.map (prims, fn {tycon = c, ...} =>
                 seq [layout c, str " size is ",
(*
                      (*Int.layout*)Int_layout (MLton.size c),
*)
		      str "??",
                      str " plist length is ",
                      (*Int.layout*)Int_layout (PropertyList.length (plist c))]))
   end
(* quell unused warning *)
val _ = stats

end

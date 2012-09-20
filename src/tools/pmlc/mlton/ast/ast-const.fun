(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstConst (S: AST_CONST_STRUCTS): AST_CONST =
struct

fun Char_fromHexDigit n = String.sub ("0123456789ABCDEF", n)

open S Region.Wrap

datatype node =
   Bool of bool
 | Char of (*IntInf.t*)IntInf.int
 | Int of (*IntInf.t*)IntInf.int
 | Real of string
 | String of (*IntInf.t*)IntInf.int vector
 | Word of (*IntInf.t*)IntInf.int
type t = node Region.Wrap.t
type node' = node
type obj = t

fun ordToString (c: (*IntInf.t*)IntInf.int): string =
      let
         fun loop (n: int, c: (*IntInf.t*)IntInf.int, ac: char list) =
            if n = 0
               then implode ac
            else
               let
                  val (q, r) = IntInf.quotRem (c, 0x10)
               in
                  loop (n - 1, q, (*Char.fromHexDigit*)Char_fromHexDigit ((*Int.fromIntInf*)Int.fromLarge r) :: ac)
               end
         fun doit (n, esc) = concat ["\\", esc, loop (n, c, [])]
      in
         if c <= 0xFF
            then (*Char.escapeSML*)Char.toString ((*Char.fromInt*)Char.chr ((*Int.fromIntInf*)Int.fromLarge c))
         else if c <= 0xFFFF
            then doit (4, "u")
         else doit (8, "U")
      end

local
   open Layout
in
   fun layout c =
      case node c of
         Bool b => if b then str "true" else str "false"
       | Char c => str (concat ["#\"", ordToString c, "\""])
       | Int s => str (IntInf.toString s)
       | Real l => (*String.layout*)Layout.str l
       | String s =>
            str (concat ["\"", concat (MLtonVector.toListMap (s, ordToString)), "\""])
       | Word w => str (concat ["0wx", (*IntInf.format (w, StringCvt.HEX)*)IntInf.fmt StringCvt.HEX w])
end

end

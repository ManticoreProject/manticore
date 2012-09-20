(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AST_CONST_STRUCTS =
   sig
   end

signature AST_CONST =
   sig
      include AST_CONST_STRUCTS

      type t
      datatype node =
         Bool of bool
       | Char of (*IntInf.t.*)IntInf.int
       | Int of (*IntInf.t.*)IntInf.int
       | Real of string
       | String of (*IntInf.t.*)IntInf.int vector
       | Word of (*IntInf.t.*)IntInf.int
      include WRAPPED sharing type node' = node
                      sharing type obj = t

      val layout: t -> Layout.t
      val ordToString: (*IntInf.t.*)IntInf.int -> string
   end

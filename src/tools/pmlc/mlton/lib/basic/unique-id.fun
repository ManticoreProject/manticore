(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor UniqueId():> UNIQUE_ID =
   struct
      type t = unit ref

      fun new(): t = ref()

      val equals = Ref.equals

      fun layout _ = Layout.empty

      fun toString _ = ""
   end

functor UnitUniqueId():> UNIQUE_ID =
   struct
      open Unit

      fun new() = ()

      fun toString _ = ""
   end

functor IntUniqueId():> UNIQUE_ID =
   struct
      open Int

      type t = int (* [PML] *)

      val cur: t ref = ref 0

      fun new(): t = ((*Int.inc cur*) cur := !cur+1; !cur)

      fun equals (a : int, b) = (a = b)

      val layout = Layout.str o toString
   end

(* Copyright (C) 2009 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Version =
   struct
      val version = "r7526M"
      val buildDate = "Tue Jun  7 08:03:55 CDT 2011"
      val buildNode = "boot.cs.uchicago.edu"

      val banner = concat ["MLton ", version,
                           " (built ", buildDate,
                           " on ", buildNode, ")"]
   end

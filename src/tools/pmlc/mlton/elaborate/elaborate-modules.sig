(* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_MODULES_STRUCTS =
   sig
     include ELABORATE_COMMON
   end

signature ELABORATE_MODULES =
   sig
      include ELABORATE_MODULES_STRUCTS

      (* Elaborate Topdec in env, returning Core ML decs and a
      BOMEnv. While Env.t is mutable, BOMEnv.t is not, and so it needs
      to be kept and passed to the next iteration. *)

      val elaborateTopdec:
        Ast.Topdec.t * {env: Env.t, bomEnv: BOMEnv.t} -> (Decs.t * BOMEnv.t)
      val reportSequenceNonUnit: unit -> unit
      val reportUndeterminedTypes: unit -> unit
      val reportUnresolvedFlexRecords: unit -> unit
      val resolveOverloads: unit -> unit
   end

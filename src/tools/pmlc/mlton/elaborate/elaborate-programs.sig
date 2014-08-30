(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_PROGRAMS_STRUCTS =
   sig
      include ELABORATE_COMMON
   end

signature ELABORATE_PROGRAMS =
   sig
      include ELABORATE_PROGRAMS_STRUCTS

      val elaborateProgram:
        Ast.Program.t * {env: Env.t, bomEnv: BOMEnv.t} -> Decs.t
   end

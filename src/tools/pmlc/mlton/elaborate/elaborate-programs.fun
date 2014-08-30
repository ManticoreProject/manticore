(* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ElaboratePrograms (S: ELABORATE_PROGRAMS_STRUCTS): ELABORATE_PROGRAMS =
struct

structure List = MLtonList

open S

local
   open Control.Elaborate
in
   val resolveScope = fn () => current resolveScope
end

structure ElaborateModules = ElaborateModules (S)



fun elaborateProgram (program, {env = E: Env.t, bomEnv: BOMEnv.t}) =
   let
      val Ast.Program.T decs = Ast.Program.coalesce program
      fun elabTopdec (d, bEnv) = ElaborateModules.elaborateTopdec (
        d, {env = E, bomEnv = bEnv})
      (* fun elabDecAndEnrichEnv (dec, bEnv) =  *)
      (*   let  *)
      (*     val (newDecs, newEnv) =  *)
      (* we throw away the BOMEnv after this, since we no longer need it *)
      val (decs, _) =
         List.fold (decs, (Decs.empty, bomEnv), fn (ds, (decs, bEnv)) =>
                    List.fold (ds, (decs, bEnv), fn (d, (decs, bEnv')) =>
                      let
                        val (newDecs, newEnv) = elabTopdec (d, bEnv')
                      in
                        (Decs.append (decs, newDecs), newEnv)
                      end))
      val () =
         case resolveScope () of
            Control.Elaborate.ResolveScope.Program =>
               (ElaborateModules.reportUnresolvedFlexRecords ()
                ; ElaborateModules.resolveOverloads ())
          | _ => ()
   in
      decs
   end

end

(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_PROGRAMS_STRUCTS =
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      structure CoreBOM: CORE_BOM (* [PML] *)
      structure Decs: DECS
      structure Env: ELABORATE_ENV
      structure BOMEnv: ELABORATE_BOMENV (* [PML] *)
      sharing Ast = Env.Ast
      sharing Ast.Tyvar = CoreML.Tyvar
      sharing CoreML = Decs.CoreML = Env.CoreML
      sharing Decs = Env.Decs
      sharing CoreBOM.Ast = Ast (* [PML] *)
      sharing BOMEnv.CoreBOM = CoreML.CoreBOM = CoreBOM (* [PML] *)
      sharing BOMEnv.Env = Env (* [PML] *)
   end

signature ELABORATE_PROGRAMS =
   sig
      include ELABORATE_PROGRAMS_STRUCTS

      val elaborateProgram: Ast.Program.t * {env: Env.t, bomEnv: BOMEnv.t} -> Decs.t
   end

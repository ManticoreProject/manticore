(* signature ELABORATE_BOM_STRUCTS = *)
(*   sig *)
(*     structure Ast: AST *)
(*     structure CoreML: CORE_ML *)
(*     structure CoreBOM: CORE_BOM *)
(*     structure Decs: DECS *)
(*     structure Env: ELABORATE_ENV *)
(*     sharing Ast = Env.Ast *)
(*     sharing Ast.Tyvar = CoreML.Tyvar *)
(*     sharing CoreML = Decs.CoreML = Env.CoreML *)
(*     sharing Decs = Env.Decs *)
(*     ??? *)
(*   end *)

(* signature ELABORATE_BOM = *)
(*   sig *)
(*     include ELABORATE_BOM_STRUCTS *)

(*   structure AstBOM : AST_BOM *)


(*   val elaborateTopdec : Ast.Topdec.t * {env: Env.t, bomEnv: BOMEnv.t} -> Decs.t *)
(*   val elaborateStrdec: Ast.Strdec.t * {env: Env.t, bomEnv: BOMEnv.t} -> Decs.t *)

(* end *)

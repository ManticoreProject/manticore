(* translate.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature TRANSLATE_STRUCTS =
  sig
      include SXML_STRUCTS
  end

functor Translate (S: SXML_STRUCTS) : sig

    val translate : S.Program.t -> BOM.module

end = struct

structure B = BOM
structure BTy = BOMTy
structure BV = BOM.Var

fun translate sxml = let
    val name = Atom.atom "main"
    val f = BV.new ("f", BTy.T_Any)
    val body = B.E_Ret []
    val bodyLam = B.FB {f=f,
                        params=[],
                        exh=[],
                        body=B.E_Pt (ProgPt.new (), body)}
    val _ = print "Doing SXML->BOM translation\n"
in
    B.MODULE {name = name ,
              externs = [],
              hlops = [],
              rewrites = [],
              body = bodyLam}
end
    
(*    val translate = BasicControl.mkKeepPass {
	              preOutput = fn (outS, (_, ast)) => PrintAST.outputExp(outS, ast),
	              preExt = "sxml",
	              postOutput = PrintBOM.output,
	              postExt = "bom",
	              passName = "translate",
	              pass = translate,
	              registry = TranslateControls.registry
	              }*)

end

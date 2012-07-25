structure PrintDOT : sig

    val output : (TextIO.outstream * CFG.module) -> unit

end = struct

fun output (outf, CFG.MODULE {code, ...}) = let

    fun callees code : (CFG.label * CFG.label list) list = let
	(* return the outgoing targets of a function *)
	fun nodeFromBlock (CFG.BLK{lab, exit, ...}) =
            (case CFACFG.labelsOf exit
              of NONE => (lab, [])
               | SOME ls => (lab, CFG.Label.Set.listItems ls)
            (* end case *))
	fun toNode (CFG.FUNC{start,body,...}) =
            List.foldr (fn (b,rr) => (nodeFromBlock b)::rr) [] (start::body)
    in
	List.foldr (fn (node,rr) => (toNode node)@rr) [] code
    end

    fun dot callees : string = let
	fun tr c = if c = #"<" then "&lt;" else if c = #">" then "&gt;" else Char.toString c
	val toS = String.translate tr o VarRep.toString
	fun edgeToString caller (callee, acc) = caller :: " -> \"" :: toS callee :: "\";\n" :: acc
	fun edges alist = List.foldr (fn ((caller, callees), acc) => List.foldl (edgeToString ("\"" ^ toS caller ^ "\"")) acc callees) ["}\n"] alist
    in
	String.concat ("digraph {\n" :: edges callees)
    end
in
    TextIO.output (outf, dot (callees code))
end

end

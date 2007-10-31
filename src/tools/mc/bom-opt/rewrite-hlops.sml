(* rewrite-hlops.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RewriteHLOps : sig

    (* replace the high-level operators in the module with their
     * definitions; returns NONE if there was no change to the module.
     *)
    val rewrite : BOM.module -> BOM.module option

end = struct

    structure B = BOM
    structure BTy = BOMTy
    structure BU = BOMUtil
    structure H = HLOp
    structure ATbl = AtomTable
    structure RW = Rewrites

    (* ____________________________________________________________ *)
    (* findHLOps() - Find the set of HLOps in a given BOM module. *)
    fun findHLOps (module as B.MODULE{name, externs, body}) = let

        val hlopEnv = ATbl.mkTable (32, Fail "hlopEnv")

        fun findHLOpsInExp (e as B.E_Pt(_, t)) = (case t
            of B.E_Let(lhs, e1, e2) => (findHLOpsInExp e1; findHLOpsInExp e2)
             | B.E_Stmt(lhs, rhs, e) => findHLOpsInExp e
             | B.E_Fun(fbs, e) =>
               (List.app findHLOpsInLambda fbs;
                findHLOpsInExp e)
             | B.E_Cont(fb, e) => (findHLOpsInLambda fb; findHLOpsInExp e)
             | B.E_If(x, e1, e2) => (findHLOpsInExp e1; findHLOpsInExp e2)
             | B.E_Case(x, cases, dflt) =>
               (List.app (fn (p, e) => findHLOpsInExp e) cases;
                case dflt
                 of SOME e => findHLOpsInExp e
                  | NONE => ())
             | B.E_Apply _ => ()
             | B.E_Throw _ => ()
             | B.E_Ret _ => ()
             | B.E_HLOp(hlOp, args, rets) =>
               ATbl.insert hlopEnv (H.name hlOp, hlOp)
            (* end case *))
        and findHLOpsInLambda (B.FB{f, params, exh, body}) =
            (findHLOpsInExp body)

    in
        findHLOpsInLambda body;
        hlopEnv
    end (* findHLOps *)

    (* ____________________________________________________________ *)
    (* rewrite() - Rewrite the given BOM module, using HLOp rewrites in the
       library path. *)
    fun rewrite (module as B.MODULE{name, externs, body}) = let
	val changed = ref false
        (* __________________________________________________ *)
        (* XXX Not sure rewrites need to worry about this stuff
        inherrited from the HLOp expander (unless we add C function
        calls to the rewrites). *)
        val importEnv = let
            val importEnv = ATbl.mkTable (32, Fail "importEnv")
            fun ins (cf as CFunctions.CFun{name, ...}) =
                ATbl.insert importEnv (Atom.atom name, cf)
        in
            List.app ins externs;
            importEnv
        end

        val nExterns = ATbl.numItems importEnv

        fun getExterns () =
            if (nExterns = ATbl.numItems importEnv)
            then externs
            else ATbl.listItems importEnv
        (* __________________________________________________ *)
        val hlopEnv = findHLOps module

        val hlrwFiles =
            List.concat (List.map (fn (_, hlop) => HLRWDefLoader.load hlop)
                                  (ATbl.listItemsi hlopEnv))

        val hlrws = List.concat hlrwFiles

        val hlrwGrammar =
            foldl Rewrites.addRWToGrammar (Rewrites.newGrammar ()) hlrws

    in
        (* DEBUG: print (Rewrites.grammarToString hlrwGrammar); *)
	if !changed
	then SOME(B.mkModule(name, getExterns(), body))
	else NONE
    end

end (* RewriteHLOps struct *)

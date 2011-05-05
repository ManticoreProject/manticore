(* inline-ranges.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Inline range expressions.
 * 
 * Range inlining can be turned on and off with -Cast.inline-ranges-on=<bool> 
 *
 * Range inlining is a "keep pass" controlled by -Cast.keep-inline-ranges=<bool>
 *
 * By default range inlining is on and keeping is off.                           
 *
 * Searches for variables bound to range expressions, and, having found them,
 *   inlines their range expressions at uses.
 * 
 * This is done at this early point in compilation since we want ranges inlined
 *   into parallel comprehensions early for fusion from maps to tabs.
 * 
 * The transformation is roughly as follows:
 *   
 *   val x = [| 1+1 to 100*100 by 3*3 |]
 *   val y = [| f n | n in x |]
 * ===>
 *   val xLo = 1+1
 *   val xHi = 100*100
 *   val xStep = 3*3
 *   val x = [| xLo to xHi by xStep |]
 *   val y = [| f n | n in [| xLo to xHi by xStep |] |]
 *
 * It is to be expected that x will likely be removed by a later optimization 
 *   as an unused variable.
 *
 * Some qualifications: 
 *
 * - variables such as xLo, xHi and xStep in the example are not created if
 *   they would be bound to constants or variables
 *
 * - variables in tuples one level deep are found and subject to the optimization;
 *   deeper than that, they aren't. In what follows, r0's range will be inlined at
 *   its uses, while r1's won't.
 *
 *     val (r0, x) = ([|1 to 100|], false)
 *     val ((r1, y), z) = (([|1 to 1000|], false), 0)
 *
 *   Certainly this can be done, but it currently isn't.
 *
 * - variables inside constructor patterns such as
 *
 *     val SOME(r) = SOME([|a to b|])
 *
 *   are also not found.
 *
 * More work is required to make this more complete, but in its current state 
 *   this pass catches ranges in many common usage patterns.
 *
 *  - Adam Shaw, May 2011
 *)

structure InlineRanges : sig

  val inlineRanges : AST.exp -> AST.exp

end = struct

  fun println s = (print s; print "\n")

  structure A = AST
  structure B = Basis
  structure T = Types
  structure AU = ASTUtil
  structure VT = Var.Tbl

(* While walking the tree, we maintain a map from all those vars that are      *)
(*   bound to a range to the range expressions to which they are bound.        *)
(* The env is imperative since it persists from one inlining pass to the next. *)
(* Rationale: its information remains accurate across passes, and one doesn't  *)
(*   want to recompute it on every pass needlessly.                            *)
  type env = A.exp VT.hash_table
  fun emptyEnv () : env = VT.mkTable (32, Fail "InlineRanges.env")

(* +debug *)
  fun range_tos (A.RangeExp (e1, e2, o3, t)) = let
        fun tos (A.VarExp (x, _)) = Var.nameOf x
	  | tos (A.ConstExp (A.LConst (lit, t'))) = Literal.toString lit
	  | tos _ = raise Fail "unexpected form in range"
        val step = (case o3
          of NONE => ""
	   | SOME e3 => " by " ^ tos e3
          (* end case *))
        in
          String.concat ["[|", tos e1, " to ", tos e2, step, "|]"]
        end
    | range_tos _ = raise Fail "not a range"
  fun printEnv env = let
    fun tos env = let
      val xrs = VT.listItemsi env
      fun s (x, r) = Var.nameOf x ^ "-->" ^ range_tos r
      in
        String.concatWith ";" (List.map s xrs)
      end
    in
      println ("{" ^ tos env ^ "}")
    end
(* -debug *)

  fun constOrVar e = (case e
    of A.ConstExp _ => true
     | A.VarExp _ => true
     | _ => false
    (* end case *))

  fun id x = x

  infix oo
  fun f oo g = fn (x, y) => (f x, g y)

  val inlineRangesInternal : A.exp -> A.exp = let
    val changed = ref false
    fun resetChanged () = changed := false
    fun noteChange x = (changed := true; x)
    val env = emptyEnv () (* will be destructively modified as we go *)
    val extendEnv = VT.insert env
    val $ = List.map
    fun exp e = (case e
      of A.LetExp (b, e) => letExp (b, e)
       | A.IfExp (e1, e2, e3, t) =>
           A.IfExp (exp e1, exp e2, exp e3, t)
       | A.CaseExp (e, ms, t) => A.CaseExp (exp e, $match ms, t)
       | A.PCaseExp (es, ms, t) =>
           A.PCaseExp ($exp es, $pmatch ms, t)
       | A.HandleExp (e, ms, t) => A.HandleExp (exp e, $match ms, t)
       | A.RaiseExp (e, t) => A.RaiseExp (exp e, t)
       | A.FunExp (x, e, t) => A.FunExp (x, exp e, t)
       | A.ApplyExp (e1, e2, t) => A.ApplyExp (exp e1, exp e2, t)
       | v as A.VarArityOpExp _ => v
       | A.TupleExp es => A.TupleExp ($exp es)
       | A.RangeExp (e1, e2, optE3, t) =>
           A.RangeExp (exp e1, exp e2, Option.map exp optE3, t)
       | A.PTupleExp es => A.PTupleExp ($exp es)
       | A.PArrayExp (es, t) => A.PArrayExp ($exp es, t)
       | A.PCompExp (e, pes, optE) =>
           A.PCompExp (exp e, $(id oo exp) pes, Option.map exp optE)
       | A.PChoiceExp (es, t) => A.PChoiceExp ($exp es, t)
       | A.SpawnExp e => A.SpawnExp (exp e)
       | c as A.ConstExp _ => c
       | ve as A.VarExp (x, ts) => (case VT.find env x
           of SOME range => noteChange range
  	    | NONE => ve
  	   (* end case *))
       | A.SeqExp (e1, e2) => A.SeqExp (exp e1, exp e2)
       | ov as A.OverloadExp _ => ov
       | A.ExpansionOptsExp (opts, e) => A.ExpansionOptsExp (opts, exp e)
       | p as A.PArrayOp _ => p
       | A.FTupleExp es => A.FTupleExp ($exp es)
       | A.FArrayExp (es, s, t) => A.FArrayExp ($exp es, ntree s, t)
       | f as A.FlOp _ => f
      (* end case *))
    and ntree n = (case n
      of A.Lf (e1, e2) => A.Lf (exp e1, exp e2)
       | A.Nd ns => A.Nd ($ntree ns)
      (* end case *))
    and lambda (A.FB (f, x, b)) = A.FB (f, x, exp b)
    and match m = (case m
      of A.PatMatch (p, e) => A.PatMatch (p, exp e)
       | A.CondMatch (p, e1, e2) => A.CondMatch (p, exp e1, exp e2)
      (* end case *))
    and pmatch m = (case m
      of A.PMatch (ps, e) => A.PMatch (ps, exp e)
       | A.Otherwise (ts, e) => A.Otherwise (ts, exp e)
      (* end case *))
    and letExp (b, e) = (case b
      of A.ValBind (p, rhs) => AU.mkLetExp (valBind A.ValBind (p, exp rhs), exp e)
       | A.PValBind (p, rhs) => AU.mkLetExp (valBind A.PValBind (p, exp rhs), exp e)
       | A.FunBind lams => A.LetExp (A.FunBind ($lambda lams), exp e)
       | A.PrimVBind _ => A.LetExp (b, exp e)
       | A.PrimCodeBind _ => A.LetExp (b, exp e)
      (* end case *))
    and valBind bindForm (p, rhs) = (case p
      of A.VarPat x => (case rhs
           of A.RangeExp (e1, e2, o3, t) => let
                val (binds, rng') = rangeBind bindForm (x, e1, e2, o3, t)
                in
		  binds @ [bindForm (p, rng')]
	        end
	    | A.VarExp (y, ts) => (case VT.find env y
                of SOME range => let
                     val _ = extendEnv (x, range)
		     val _ = noteChange ()
                     in
		       [bindForm (p, range)]
		     end
		 | NONE => [bindForm (p, exp rhs)]
                (* end case *))				  
	    | notRange => [bindForm (p, exp notRange)]
           (* end case *))
       | A.TuplePat ps => (case rhs
          of A.TupleExp es => let
               fun lp ([], [], es', binds) = let
		     val rhs' = A.TupleExp (List.rev es')
                     in
                       binds @ [bindForm (p, exp rhs')]
	             end 
		 | lp (p::ps, e::es, es', binds) = (case (p, e)
                     of (A.VarPat x, A.RangeExp (e1, e2, o3, t)) => let
                          val (binds', rng') = rangeBind bindForm (x, e1, e2, o3, t)
                          in
			    lp (ps, es, rng'::es', binds@binds')
			  end
		      | _ => lp (ps, es, e::es', binds)
                     (* end case *))
		 | lp _ = raise Fail "tuple pat matched against tuple exp of different arity"
               in
                 lp (ps, es, [], [])
	       end
	   | _ => [bindForm (p, exp rhs)]
          (* end case *))
       | _ => [bindForm (p, exp rhs)]
      (* end case *))
    and rangeBind bindForm (x, e1, e2, o3, t) = let
      fun maybeBind (oldBinds, expBindCand, varName, k) =
            if constOrVar expBindCand then
              (* don't introduce a new var for a const or var *)
              (oldBinds, k expBindCand)
  	    else let
              val var = Var.new (varName, t)
  	      val bind = bindForm (A.VarPat var, expBindCand)
  	      val exp = A.VarExp (var, [])
              in
                (bind::oldBinds, k exp)
              end
      in (case VT.find env x
        of SOME range => (* we've already caught this one *)
             ([], range)
  	 | NONE => let  		 
             fun name suff = Var.nameOf x ^ suff
             val (binds3, o3') = (case o3
  	       of NONE => ([], NONE)
  		| SOME e3 => maybeBind ([], e3, name "Step", SOME)
               (* end case *))
  	     val (binds2, e2') = maybeBind (binds3, e2, name "Hi", id)
  	     val (binds1, e1') = maybeBind (binds2, e1, name "Lo", id)
  	     val rng' = A.RangeExp (e1', e2', o3', t)
             in
               extendEnv (x, rng');
  	       (binds1, rng')
             end
        (* end case *))
      end
    fun iterateTillFixed i e = let
(* (\* +debug *\) *)
(*       val _ = println ("InlineRanges: iteration " ^ Int.toString i) *)
(*       val _ = printEnv env *)
(* (\* -debug *\) *)
      val _ = resetChanged ()
      val e' = exp e
      in
        if !changed then iterateTillFixed (i+1) e' else e'
      end
    in
      iterateTillFixed 1
    end
 
  val inlineRanges : A.exp -> A.exp = BasicControl.mkKeepPassSimple {
    output = PrintAST.outputExpNoTypes,
    ext = "ast",
    passName = "inline-ranges",
    pass = inlineRangesInternal,
    registry = ASTOptControls.registry
  }

end

(* TODO: I catch ranges bound in tuples: *)
(*   val (x, y) = (rangeExp, foo) *)
(*   val z = x (* --> rangeExp *) *)
(* But I don't catch ranges bound in nested tuples. *)
(* Not critical, but noted. *)

(* TODO: I think I could also catch these for inlining... *)
(*   val x = (e1; e2; ...; rangeExp) *)
(*   val y = x (* --> rangeExp *) *)
(* Doesn't seem critical. *)


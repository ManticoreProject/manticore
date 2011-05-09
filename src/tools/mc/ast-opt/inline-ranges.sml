(* inline-ranges.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Inline range expressions.
 * 
 * Searches for variables bound to range expressions, and, having found them,
 *   inlines their range expressions at uses.
 * 
 * This is done at this early point in compilation since we want ranges inlined
 *   into parallel comprehensions early for fusion from maps to tabulates.
 *
 * Range inlining can be turned on and off with -Cast.inline-ranges=<bool> 
 *
 * Range inlining is a "keep pass" controlled by -Cast.keep-inline-ranges=<bool>
 *
 * By default inlining ranges is on and "keeping" is off.
 * 
 * The transformation is roughly as follows:
 *   
 *   val x = [| 1+1 to 100*100 by 3*3 |]
 *   val y = [| f n | n in x |]
 *   ===>
 *   val xLo = 1+1
 *   val xHi = 100*100
 *   val xStep = 3*3
 *   val x = [| xLo to xHi by xStep |]
 *   val y = [| f n | n in [| xLo to xHi by xStep |] |]
 *
 * It is to be expected that x will likely be removed by a later optimization 
 *   as an unused variable.
 *
 * Variables such as xLo, xHi and xStep in the example are not created if
 *   they would be bound to constants or variables. (TODO: we can raise the
 *   bar higher for when a variable is created. Binary arithmetic expressions,
 *   for example, probably don't need to be hoisted.)
 *
 * As a bonus, the inliner also catches bindings where the range is the last in a SeqExp.
 * For example, in the following program:
 *
 *   val x = (print "foo"; print "bar"; [| 1 to 100 |])
 *   val y = x
 *
 * the value [| 1 to 100 |] will be inlined on the rhs of y's binding.
 *
 *  - Adam Shaw, May 2011
 *)

structure InlineRanges : sig

  val inlineRanges : AST.exp -> AST.exp

end = struct

(* good things to have around... *)
  fun println s = (print s; print "\n")
  fun id x = x

  structure A = AST
  structure B = Basis
  structure T = Types
  structure AU = ASTUtil
  structure VT = Var.Tbl

(* In this module, an env maps vars to ranges. *)
(* The invariant that all exps in the env are ranges is dynamically enforced in "extend". *)
  type env = A.exp VT.hash_table

  val emptyEnv : unit -> env = fn _ => VT.mkTable (32, Fail "InlineRanges.env")

  fun find (env : env) : A.var -> A.exp option = VT.find env

  fun extend (env : env) : A.var * A.exp -> unit = fn (x, e) => (case e
    of A.RangeExp _ => VT.insert env (x, e)
     | _ => raise Fail "extend: trying to insert a non-range"
    (* end case *))

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

(* constOrVar : exp -> bool *)
  val constOrVar = fn (A.ConstExp _ | A.VarExp _) => true | _ => false

(* pointwise composition *)
  infix oo
  fun f oo g = fn (x, y) => (f x, g y)

(* maybe monad *)
  infix >>=
  fun opt >>= k = (case opt of SOME x => k x | NONE => NONE)

(* seqEndingRange : exp -> (exp list * exp * exp * exp * ty) option   *)
(* If a sequence (including sequence of sequences) has a RangeExp as  *)
(*   its last item, return SOME list of expressions preceding it and  *)
(*   the elements of that RangeExp. Otherwise return NONE.            *)
  local
    fun cons e (es, e1, e2, o3, t) = (e::es, e1, e2, o3, t)
  in
    fun seqEndingRange (e : A.exp) = (case e
      of A.SeqExp (e0, A.RangeExp (e1, e2, o3, t)) =>
           SOME ([e0], e1, e2, o3, t)
       | A.SeqExp (e, s as A.SeqExp _) =>
           seqEndingRange s >>= (SOME o cons e)
       | _ => NONE
      (* end case *))
  end (* local *)

(* The following boolean cell and utils will be used to iterate to fixpoint. *)
  val changed = ref false
  fun resetChanged () = changed := false
  fun noteChange x = (changed := true; x)

(* inlineRangesInternal : exp -> exp *)
  val inlineRangesInternal : A.exp -> A.exp = let
    val env = emptyEnv () (* will be destructively updated as we go *)
    val extendEnv = extend env
    val find = find env
    val $ = List.map
    fun exp e = (case e
      of A.LetExp (b, e) => AU.mkLetExp (binding b, exp e)
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
       | A.RangeExp (e1, e2, o3, t) =>
           A.RangeExp (exp e1, exp e2, Option.map exp o3, t)
       | A.PTupleExp es => A.PTupleExp ($exp es)
       | A.PArrayExp (es, t) => A.PArrayExp ($exp es, t)
       | A.PCompExp (e, pes, optE) =>
           A.PCompExp (exp e, $(id oo exp) pes, Option.map exp optE)
       | A.PChoiceExp (es, t) => A.PChoiceExp ($exp es, t)
       | A.SpawnExp e => A.SpawnExp (exp e)
       | c as A.ConstExp _ => c
       | ve as A.VarExp (x, ts) => (case find x
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
    and binding b = (case b
      of A.ValBind (p, rhs) => valBind (A.ValBind, p, exp rhs)
       | A.PValBind (p, rhs) => valBind (A.PValBind, p, exp rhs)
       | A.FunBind lams => [A.FunBind ($lambda lams)]
       | A.PrimVBind _ => [b] 
       | A.PrimCodeBind _ => [b]
      (* end case *))
    and valBind (bindForm, p, e) : A.binding list = let
      fun pat (p, rhs) : (A.binding list * A.exp) option = (case p
        of A.VarPat x => varPat (x, rhs)
	 | A.TuplePat ps => (case rhs
             of A.TupleExp es => let
		  fun f (p, e, (bs, es)) = (case pat (p, e)
                    of SOME (bs', e') => (bs' @ bs, e'::es)
		     | NONE => (bs, e::es)
                    (* end case *))
                  val (bs, es') = ListPair.foldrEq f ([], []) (ps, es)
		  in
		    SOME (bs, A.TupleExp es')
		  end
	      | _ => NONE
	     (* end case *))
(* FIXME see inline-ranges-K.pml *)
(* seems to be a flattening translation problem, not related to inlining. *)
	 | A.ConPat (_, _, p) => (case rhs
             of A.ApplyExp (dcon as A.ConstExp (A.DConst _), e, _) =>
                  pat (p, e) >>= (fn (bs, e') =>
                    SOME (bs, AU.mkApplyExp (dcon, [e'])))
	      | _ => NONE
             (* end case *))
	 | _ (* WildPat or ConstPat *) => NONE
        (* end case *))
      and varPat (x, rhs) : (A.binding list * A.exp) option = let
        fun rangeBind (e1, e2, o3, t) = let         
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
	  in (case find x
	    of SOME range => NONE (* we've already caught this one (on a previous pass) *)                    
  	     | NONE => let  		                  
                 fun name suff = Var.nameOf x ^ suff
		 val (binds3, o3') = (case o3
  		   of SOME e3 => maybeBind ([], e3, name "Step", SOME)
		    | NONE => ([], NONE)
		   (* end case *))
  		 val (binds2, e2') = maybeBind (binds3, e2, name "Hi", id)
  		 val (binds1, e1') = maybeBind (binds2, e1, name "Lo", id)
  		 val rng' = A.RangeExp (e1', e2', o3', t)
                 in
	           extendEnv (x, rng');
  	           SOME (binds1, rng')
	         end
            (* end case *))
	  end
        in (case rhs
          of A.RangeExp (e1, e2, o3, t) => rangeBind (e1, e2, o3, t)
	   | A.SeqExp (e1, e2) =>
               seqEndingRange rhs >>= (fn (es, e1, e2, o3, t) =>
                 rangeBind (e1, e2, o3, t) >>= (fn (bs, rng') =>
                   SOME (bs, List.foldr A.SeqExp rng' es)))
	   | A.VarExp (y, ts) => 
               find y >>= (fn rng =>
                 (extendEnv (x, rng); 
		  SOME ([], noteChange rng)))
	   | _ => NONE
          (* end case *))
        end
      in (case pat (p, e)
        of NONE => [bindForm (p, e)]
	 | SOME (bs, e') => bs @ [bindForm (p, e')]
        (* end case *))
      end
(* NOTE: I'm pretty sure, with recent modifications, that I get everything in one pass. *)
(* If we can prove it we can remove the iteration. - ams *)
    fun iterateTillFixed i e = let
(* (\* +debug *\) *)
(*       val _ = printEnv env *)
(*       val _ = println ("InlineRanges: iteration " ^ Int.toString i) *)
(* (\* -debug *\) *)
      val _ = resetChanged ()
      val e' = exp e
      in
        if !changed then iterateTillFixed (i+1) e' else e'
      end
    in
      iterateTillFixed 1
    end

(* inlineRanges : exp -> exp *)
  val inlineRanges : A.exp -> A.exp = BasicControl.mkKeepPassSimple {
    output = PrintAST.outputExpNoTypes,
    ext = "ast",
    passName = "inline-ranges",
    pass = inlineRangesInternal,
    registry = ASTOptControls.registry
  }

end

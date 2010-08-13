(* jfp-translate-pcase.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * NOTE: In the paper, the trans, state and act functions (blocks 5-7)
 *   are mutually recursive. They don't need to be. If the acts come first,
 *   then the states, then the transes, it works out.
 *)

(* NOTE: This is a work in progress, and not yet "live". - ams *)

structure JFPTranslatePCase (* : sig

  (* An AST to AST translation of parallel cases. *)
    val tr : (AST.exp -> AST.exp) 
             -> AST.exp list * AST.pmatch list * AST.ty 
             -> AST.exp

  end *) = struct

  fun copies n k = List.tabulate (n, fn _ => k)

  structure A = AST
  structure B = Basis
  structure R = Rope
  structure T = Types
  structure U = BasisUtil

(* mkResPat : A.pat -> A.pat *)
(* Given a pattern (p : t), produce the pattern (RES(p) : t result). *)
  fun mkResPat pat = let
    val conRES = U.resultRES ()
    val ty = TypeOf.pat pat
    in
      AST.ConPat (conRES, [ty], pat)
    end

(* mkExnPat : (AST.pat * AST.ty) -> AST.pat *)
(* Given a pattern p and type t, produce the pattern (Exn p : t result). *)  
  fun mkExnPat (pat, ty) = let
    val conEXN = U.resultEXN ()
    in
      AST.ConPat (conEXN, [ty], pat)
    end

  local
    val cCancel (* Cancel.cancel *) = U.cancelCancel ()
    val cSpawn  (* Cancel.spawn  *) = U.cancelSpawn ()
    fun v2e (v: A.var) : A.exp = A.VarExp (v, [])
  in
    fun cancel (c: A.var) : A.exp = A.ApplyExp (v2e cCancel, v2e c, B.unitTy)
    fun spawn (c: A.var, f: A.exp) : A.exp = let
      val args = A.TupleExp [A.VarExp (c, []), f]
      in
        A.ApplyExp (A.VarExp (cSpawn, []), args, Basis.unitTy)
      end
  end (* local *)

  (* bitstrings *)

  type bitstring = bool list

  datatype pcase' 
    = PCase' of A.exp list * A.pmatch list * bitstring list * A.ty

  fun mkBitstrings (AST.PCaseExp (es, ms, resTy)) : pcase' = let
    val len = List.length es
    fun bit (AST.NDWildPat _) = true
      | bit (AST.Pat _) = false
      | bit (AST.HandlePat _) = raise Fail "HandlePats not supported"
    fun lp ([], acc) = PCase' (es, ms, List.rev acc, resTy)
      | lp ((m as AST.Otherwise e)::ms, acc) = 
          (case ms
 	     of [] => lp ([], (copies len true)::acc)
	      | _  => raise Fail "otherwise not last in pcase")
      | lp ((m as PMatch(ps,e))::ms, acc) = let
          val bs = List.map bit ps
          in
	    lp (ms, bs::acc)
	  end
    in
      lp (ms, [])
    end

(* bitstringEq *)
  val bitstringEq : (bitstring * bitstring) -> bool = let
    fun eq ([], []) = true
      | eq (b::bs, c::cs) = ASTUtil.boolEq (b, c) andalso eq (bs, cs)
      | eq _ = false
    in
      eq
    end

(* bitwiseAnd: bitwise and of two bitstrings. *)
(* pre: bitstring arguments are the same length. *)
  val bitwiseAnd : (bitstring * bitstring) -> bitstring = let
    fun andFn (b1, b2) = b1 andalso b2
    in
      ListPair.mapEq andFn
    end

(* next: set the kth bit to 1 *)
  fun next (k: int, bs: bitstring) : bitstring = let
    fun lp (i, b::bs) = 
          if i > 0 then b::lp(i-1,bs)
	  else if i = 0 then true::bs
	  else raise Fail "not enough bits (" ^ Int.toString i ^ ")"
      | lp (_, []) = raise Fail "not enough bits (empty)"
    in
      lp (k, bs)
    end

(* nextSet *)
(* pre: k < len *)
(* ex: nextSet (1, 2) -> [01, 11] *)
(* ex: nextSet (1, 3) -> [010, 011, 110, 111] *)
  local 
  (* allPoss: int -> bitstring list *)
  (* Constructs all possible bitstrings of given length. *)
  (* Negative arguments don't make sense, but they are treated as 0. *)
    fun allPoss (len: int) : bitstring list = 
      if len <= 0 then [[]]
      (* mini database of common small cases (not strictly necessary) *)
      else if len = 1 then 
        [[true],[false]]
      else if len = 2 then 
	[[true, true], [true,false], [false,true], [false,false]]
      (* general case *)
      else let
        fun lp [] = []
	  | lp (bs::bss) = (true::bs)::(false::bs)::(lp bss)
        in
          lp (allPoss (len-1))
        end
  in
  (* nextSet : int * int -> bitstring list *)
  (* Computes the "next_k" set (see jfp paper for definition). *)
  (* The argument k is a zero-based index of the bit to be set (to 1). *)
  (* The argument len is the total length of the bistrings constructed. *)
  (* pre: k < len *)
  (* Uses the dreaded @. *)
    fun nextSet (k: int, len: int) : bitstring list = 
      if k >= len then 
        raise Fail "nextSet: k must be less that len"
      else let
        val bss = allPoss k (* bits before the 1 (i.e. true) *)
	val css = allPoss (len-k-1) (* bits after the 1 *)
	fun lp ([], acc) = acc
	  | lp (bs::bss, acc) = let
              val x = List.map (fn cs => bs@(true::cs)) css
              in
                lp (bss, acc@x)
	      end
	in
          lp (bss, [])
        end
  end (* local *)

(* restrict: restrict the given list of ppats by given bitstring *)
(* pre: length of both arguments is the same *)
(* ex: restrict([p1,p2,?,p4],1011) = (SOME p1, _, SOME p4) *)
  fun restrict (ps: AST.ppat list, bs: bitstring) : AST.pat list = let
    fun lp ([], [], acc) = List.rev acc
      | lp (p::ps, false::bs, acc) = lp (ps, bs, acc)
      | lp (p::ps, true::bs, acc) = 
          (case p
	     of AST.Pat p => let
		  val p' = AST.ConPat (optSOME (), [TypeOf.pat p], p)
                  in
		    lp (ps, bs, p'::acc)
                  end
	      | AST.NDWildPat ty => let
                  val p' = AST.WildPat (mkOptTy ty)
                  in
		    lp (ps, bs, p'::acc)
		  end
	      | AST.HandlePat _ => raise Fail "HandlePats not supported")
      | lp _ = raise Fail "restrict: ps and bs different lengths"
    in
      lp (ps, bs, [])
    end

(* rules *)
  fun rules (s: bitstring, c: pcase') : pcase' = let
    val PCase' (es, ms, bss, t) = c
    fun pred bs = bitstringEq (bs, bitwiseAnd (bs, s))
    fun lp ([], [], accMs, accBss) = 
	  PCase' (es, List.rev accMs, List.rev accBss, t)
      | lp (m::ms, bs::bss, accMs, accBss) = 
          if pred bs then lp (ms, bss, m::ms, bs::accBss)
	  else lp (ms, bss, accMs, accBss)
      | lp _ = raise Fail "bug: ms and bss should be same length" 
    in
      lp (ms, bss, [], [])
    end

(* avail *)
(* What it returns is actually an int set, but it's inefficient to build it as such. *)
  fun avail (s: bitstring) : int list = let
    fun lp ([], _, acc) = List.rev acc
      | lp (b::bs, k, acc) = lp (bs, k+1, if b then k::acc else acc)
    in
      lp (s, 0, [])
    end

(* boundVars *)
  fun boundVars (ps: AST.ppat list) : Var.Set.set = let
    fun pat (AST.ConPat (_, _, p)) = pat p
      | pat (AST.TuplePat ps) = List.concat (List.map pat ps)
      | pat (AST.VarPat x) = [x]
      | pat (AST.WildPat _) = []
      | pat (AST.ConstPat _) = []
    fun ppat (AST.NDWildPat _) = []
      | ppat (AST.HandlePat _) = raise Fail "HandlePat not supported"
      | ppat (AST.Pat p) = pat p
    val addList' (x, s) = Var.Set.addList (s, x)
    in
      List.foldl addList' Var.Set.empty (List.map ppat ps)
    end

(* don'tCare *)
  fun don'tCare (bs: bistring) : int list = let
    fun lp ([], _, acc) = List.rev acc
      | lp (b::bs, k, acc) = lp (bs, k+1, if b then acc else k::acc)
    in
      lp (bs, 0, [])
    end

(* cancelThese *)
(* pre: cs and bs same length *)
  fun cancelThese (cs: A.var list, bs: bitstring) : cs: A.var list = let
    fun lp ([], [], acc) = List.rev acc
      | lp (c::cs, true::bs, acc) = lp (cs, bs, acc)
      | lp (c::cs, false::bs, acc) = lp (cs, bs, c::acc)
      | lp _ = raise Fail "pre"
    in
      lp (cs, bs, [])
    end

(* see JPF paper fig. 17 for this implementation *)

(* (2)
 * we need to generate this:
 *   val state = MVar.new S_0
 * S_0 is an array of falses. We need to know its length.
 *)
  fun initState (k: int, b: A.exp) : A.exp * A.var = let
    val args = [ASTUtil.mkInt k, ASTUtil.falseExp]
    val newArr = A.VarExp (BasisUtil.arrayArray (), [Basis.boolTy])
    val s_0 = ASTUtil.mkApplyExp (newArr, args)
    in
      mvarNew ("state", s_0, b)
    end
  and mvarNew (name: string, init: A.exp, e: A.exp) : A.exp * A.var = let
    val ty = U.mvarTy (TypeOf.exp init)
    val x = Var.new (name, ty)
    val new = A.VarExp (U.mvarNew (), [Basis.boolTy])
    val rhs = ASTUtil.mkApplyExp (new, [init])
    in
      (A.LetExp (A.ValBind (x, rhs), e), x)
    end

(* (3)
 * mkRefs: given a list of types and a body expression, mkRefs produces
 *   a variable r_k for each type, each bound to a fresh ref NONE.
 * ex: mkRefs ([int, bool], b) -->
 *      (let (r1: int option ref)  = ref NONE
 *           (r2: bool option ref) = ref NONE
 *       in 
 *         b
 *       end,
 *      [r1, r2])
 *)
  fun mkRefs (ts: A.ty list, b: A.exp) : A.exp * (A.var list) = let
    fun lp ([]: A.ty list, k: int, acc: A.exp, rs: A.var list) = 
          (acc, rs)
      | lp (t::ts, k, acc, rs) = let
          val (acc', r) = mkRef (k, t, acc)
          in
	    lp (ts, k-1, acc', r::rs)
	  end
    in
      lp (List.rev ts, List.length ts, b, [])
    end
  and mkRef (k: int, ty: A.ty, b: A.exp) : A.exp * A.var = let
    val r = Var.new ("r" ^ Int.toString k, U.refTy (U.optTy ty))
    val e = A.LetExp (A.ValBind (A.VarPat r, refNONE ty), b) 
    in
      (e, r)
    end
  and refNONE ty = let
    val ref  = A.VarExp (U.refNew (), [U.optTy ty])
    val none = A.DConst (U.optNONE (), [ty])
    in
      ASTUtil.mkApplyExp (ref, none)
    end

(* (4) *)
  fun mkCancels (k: int, b: A.exp) : A.exp * (A.var list) = let
    val cancelTy = U.cancelTy ()
    val cancelNew = A.VarExp (U.cancelNew (), [])
    val new = ASTUtil.mkApplyExp (cancelNew, ASTUtil.unitExp)
    fun lp (k, acc, cs) = 
      if k < 1 then (acc, cs)
      else let
        val c = Var.new ("c" ^ Int.toString k, cancelTy)
	val acc' = A.LetExp (A.ValBind (A.VarPat c, new), acc)
        in
          lp (k-1, acc', c::cs)
        end
    in
      lp (k, b, [])
    end

(* (5) *)
  local
    val bitvecTy = raise Fail "todo" (* basis *)
    val bSet1F = raise Fail "todo" (* basis *)
    val orB = raise Fail "todo" (* basis *)
    val refSetV = U.refSet ()
    val optSOME = U.optSOME ()
    fun varExp x = A.VarExp (x, [])
  in
    fun mkTrans (k: int, argTy: A.ty, mState: A.var, r_k: A.var, 
		 stFuns: (bitstring * A.var) list)
        : A.lambda = let
      val stV = Var.new ("st", bitvecTy)
      val v = Var.new ("v", argTy)
      val bind = A.ValBind (A.VarPat stV, 
			    A.ApplyExp (A.VarExp (mTake, [bitvecTy]), 
					varExp mState,
					bitvecTy))
      val someV = A.ApplyExp (A.Const (A.DConst (optSOME, [argTy])),
			      varExp v,
			      U.optTy argTy)
      val setR = A.ApplyExp (A.VarExp (refSetV, [argTy]),
			     A.TupleExp [varExp r_k, 
					 someV])
      val stV' = Var.new ("st'", bitvecTy)					 
      val bind' = A.ValBind (A.VarPat stV',
			     A.ApplyExp (varExp bSet1F, 
					 A.TupleExp [varExp st, ASTUtil.mkInt k],
					 bitvecTy))
    (* this loop makes a big nested if statetement *)
    (* the "default" is raise Match *)
    (* FIXME only do this for states in Next_k *)
      fun lp ([]) = A.RaiseExp (A.ConstExp (A.DConst (B.exnMatch, [])), ty)
	| lp ((bs,stFun)::t) = let
          (* FIXME only do this if bs is in Next_k *)
            val test = raise Fail "st' = bs"
	    val app = A.ApplyExp (varExp stFun, Literal.unitLit, ty)
            in
              A.IfExp (test, app, lp t, ty)
	    end
      val cond = lp stFuns
      val body = A.LetExp (bind,
			   A.SeqExp (setR,
				     A.LetExp (bind',
					       cond)))
      val trans_k = Var.new ("trans_" ^ Int.toString k,
			     T.FunTy (argTy, ty))
      in
        A.FB (trans_k, v, body)
      end
  end (* local *)
  fun allTrans (mState: A.var, 
		ers: (A.exp * A.var) list,
		stFuns: (bitstring * A.var list)) 
      : A.lambda list = let
    fun lp (_, [], acc) = List.rev acc
      | lp (k, (e,r)::t, acc) = let
          val trans_k = mkTrans (k, TypeOf.exp e, mState, r, stFuns)
          in
            lp (k+1, t, trans_k::acc)
	  end
    in
      lp (1, ers, [])
    end

(* (7) *)
(* FIXME make this *recursive* (see +++ below) *)
  fun mkActs (env: VarSubst.subst) 
	     (pc: pcase', returnV: A.var, reraiseV: A.var, cs: A.var list) 
      : A.lambda list = let
    val varType x = TypeOf.monoTy (Var.typeOf x)
    val (PCase' (es, ms, bs, ty)) = pc
    val len = List.length ms
    fun newUnitV () = Var.new ("u", Basis.unitTy)
    fun newExV () = Var.new ("ex", Basis.exnTy)
    fun mkReraise exV = 
      ASTUtil.mkApplyExp (AST.VarExp (reraiseV, [TyVar.new (Atom.atom "'e")]),
			  AST.VarExp (exV, []))
    fun mkHandle (e, exV, rer) = A.HandleExp (e, [A.PatMatch (A.VarPat exV, rer)], ty)
    fun mkLam (actV, arg, cs, e) = let
  (* +++ this might be the right place to make this recursive... *)
  (* specifically, to transform pcases within e, recursively *)
      val exV = newExV ()
      val rer = mkReraise exV
      val h = mkHandle (e, exV, rer)
      val b = AST.ApplyExp (AST.VarExp (returnV, []), h, ty)
      val b' = List.foldr (fn (c, e) => A.SeqExp (cancel c, e)) cs b
      in
        A.FB (actV, arg, b')
      end      
    fun lp (_, [], _, acc) = List.rev acc
      | lp (j, A.Otherwise(e)::ms, _, acc) =
          (case ms 
	     of [] => let
                  val act_j = Var.new ("act" ^ Int.toString j, 
				       T.FunTy (B.unitTy, ty))
		  val e' = VarSubst.applySubst env e
		  val lam = mkLam (act_j, u, [], e')
		  in
		    lp (j+1, [], lam::acc)
		  end
	      | _ => raise Fail "otherwise is not last"
	    (* end case *))
      | lp (j, A.PMatch (ps, e)::ms, b::bs, acc) = let
          val ps' = restrict (ps, b)
          val xs = Var.Set.listItems (boundVars ps')
          val ys = List.map (fn v => Var.new (Var.nameOf v, varType v)) xs
	  val env' = ListPair.foldl VarSubst.add' env (xs, ys)
          val (argTy, e') = 
            (case List.map varType xs
	       of [] => (B.unitTy, e)
		| [t] => (t, VarSubst.applySubst env' e)
		| ts => let
                    val bind = A.ValBind (A.TuplePat (List.map A.VarPat ys),
					  A.VarExp (arg, []))
                    val e' = A.LetExp (bind, VarSubst.applySubst env' e)
		    in
                      (T.TupleTy ts, e')
                    end
	      (* end case *))
	  val argV = Var.new ("arg", argTy)
          val act_j = Var.new ("act" ^ Int.toString j, T.FunTy (argTy, ty))
	  val lam = mkLam (act_j, arg, cancelThese (cs, b), e')
          in
	    lp (j+1, [], lam::acc)
          end
    in
      lp (0, ms, [])
    end

(* (8) *)
  fun mkReraise (reraise: A.var, exnReturn: A.var, cs: A.var list, ty: A.ty) = let
    val x = Var.new ("x", Basis.exnTy)
    val etys = [TyVar.new (Atom.atom "'e")]
    val exnReturnX = 
      A.ApplyExp (A.VarExp (exnReturn, etys), A.VarExp (x, []), ty)
    val body = 
      List.foldr (fn (c,b) => A.SeqExp (cancel c, b)) exnReturnX (List.rev cs) 
    in
      A.FB (reraise, x, body)
    end

(* (9) *)
(* cs = list of cancel vars  *)
(* ts = list of trans_k vars *)
(* es = list of expressions  *)
  fun mkSpawns (cs: A.var list, ts: A.var list, es: A.exp list, actExnV: A.var) =
    fun lp ([], [], [], b) = b
      | lp (c::cs, t::ts, e::es, b) = let
          val exV = Var.new ("ex", Basis.exnTy)
          val m   = AST.PatMatch (AST.VarPat exV,
				  AST.ApplyExp (AST.VarExp (actExnV, []),
						AST.VarExp (exnV, []),
						TypeOf.exp e))
          val h   = AST.HandleExp (e, [m], TypeOf.exp e)
          val f   = thunk (AST.ApplyExp (AST.VarExp (t, []), h, raise Fail "ty?")
          val s   = spawn (c, f)
          in
            A.SeqExp (s, lp (cs, ts, es, b))
          end
      | lp _ = raise Fail "precondition: cs, ts, es should all be the same length"
    val dispatch = raise Fail "todo"
    in
      lp (cs, ts, es, dispatch)
    end
  and thunk e = AST.FunExp (Var.new ("u", Basis.unitTy), e, TypeOf.exp e)

(* --- some tests follow --- *)

(*
  structure T = TestUtils

  val zero = T.int 0
  val one  = T.int 1

  fun t (A.PCaseExp (es, pms, ty)) = tr (fn e => e) (es, pms, ty)
    | t _ = raise Fail "expecting a pcase"

  val c0 = A.PCaseExp ([zero],
		       [A.Otherwise one],
		       Basis.intTy)

  val c1 = A.PCaseExp ([zero, zero],
		       [A.PMatch ([T.intPPat 1, A.NDWildPat Basis.intTy], zero),
 		        A.Otherwise one],
		       Basis.intTy)

  val c2 = let
    val n = Var.new ("n", Basis.intTy)
    val m = Var.new ("m", Basis.intTy)
    in
      A.PCaseExp ([A.VarExp (n, []), A.VarExp (m, [])],
		  [A.PMatch ([T.intPPat 1, A.NDWildPat Basis.intTy], zero),
		   A.PMatch ([A.NDWildPat Basis.intTy, T.intPPat 2], zero),
		   A.Otherwise one],
		  Basis.intTy)
    end

(*  link time error:
  val c3 = A.PCaseExp ([zero],
		       [A.Otherwise (t c0)],
		       Basis.intTy)
*)
  fun mkTest pcase = (PrintAST.printExpNoTypes pcase;
		      PrintAST.printComment "-->";
		      PrintAST.printExpNoTypes (t pcase))

  fun test 0 = mkTest c0
    | test 1 = mkTest c1
    | test 2 = mkTest c2
(*  | test 3 = mkTest c3 *)
    | test _ = print "No such test.\n"
*)

end
    


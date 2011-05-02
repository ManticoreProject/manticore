(* jfp-translate-pcase.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * NOTE: In the paper, the trans, state and act functions (blocks 5-7)
 *   are mutually recursive. They don't need to be. If the acts come first,
 *   then the states, then the transes, it works out.
 *   As such the order of the translated components is "jumbled" here.
 *)

(* NOTE: This is a work in progress, and not yet "live". - ams *)
(* There are some holes remaining, each marked FIXME. *)

structure JFPTranslatePCase = struct

(* utilities *)
  val const   = (fn k => (fn _ => k))
  val curry   = (fn f => (fn x => fn y => f (x, y)))
  val uncurry = (fn f => (fn (x, y) => f x y))
  val copies  = (fn n => (fn k => List.tabulate (n, const k)))

(* module nicknames *)
  structure A = AST
  structure B = Basis
  structure R = Rope
  structure T = Types

  structure DB = DelayedBasis
  structure DT = DB.TyCon
  structure DD = DB.DataCon
  structure DV = DB.Var
  structure DTy = DB.Ty

(* freshAlpha : unit -> T.ty *)
  fun freshAlpha () = T.VarTy (TyVar.new (Atom.atom "'a"))

(* thunk : A.exp -> A.exp *)
  fun thunk e = A.FunExp (Var.new ("u", Basis.unitTy), e, TypeOf.exp e)

(* mkResPat : A.pat -> A.pat *)
(* Given a pattern (p : t), produce the pattern (RES(p) : t result). *)
  fun mkResPat pat = let
    val conRES = DD.resultRes ()
    val ty = TypeOf.pat pat
    in
      AST.ConPat (conRES, [ty], pat)
    end

(* mkExnPat : (AST.pat * AST.ty) -> AST.pat *)
(* Given a pattern p and type t, produce the pattern (EXN(p) : t result). *)  
  fun mkExnPat (pat, ty) = let
    val conEXN = DD.resultEXN ()
    in
      AST.ConPat (conEXN, [ty], pat)
    end

  local
    val cCancel (* Cancel.cancel *) = DV.cancelCancel ()
    val cSpawn  (* Cancel.spawn  *) = DV.cancelSpawn ()
    fun v2e (v: A.var) : A.exp = A.VarExp (v, [])
  in
  (* cancel : A.var -> A.exp *)
    fun cancel (c: A.var) : A.exp = A.ApplyExp (v2e cCancel, v2e c, B.unitTy)
  (* spawn  : A.var * A.exp -> A.exp *)    
    fun spawn (c: A.var, f: A.exp) : A.exp = let
      val args = A.TupleExp [A.VarExp (c, []), f]
      in
        A.ApplyExp (A.VarExp (cSpawn, []), args, Basis.unitTy)
      end
  end (* local *)

(* bitstrings *)
(* bistrings are represented as lists of bools as long as possible; if they *)
(*   are needed in generated code, they can be manifest as AST bitstrings via *)
(*   the function bitstringToAST (below) *)
  type bitstring = bool list

(* a pcase' wraps the components of a pcase, plus
 * each pmatch is tagged with the corres. bitstring 
 *)
  datatype pcase' 
    = PCase' of A.exp list * A.pmatch list * bitstring list * A.ty

(* mkBitstrings : A.exp list * A.pmatch list * A.ty -> pcase' *)
  fun mkBitstrings (es: A.exp list, ms: A.pmatch list, resTy: A.ty) : pcase' = let
    val len = List.length es
    fun bit (AST.NDWildPat _) = true
      | bit (AST.Pat _) = false
      | bit (AST.HandlePat _) = raise Fail "HandlePats not supported"
    fun lp ([], acc) = PCase' (es, ms, List.rev acc, resTy)
      | lp ((m as AST.Otherwise e)::ms, acc) = 
          (case ms
 	     of [] => lp ([], (copies len true)::acc)
	      | _  => raise Fail "otherwise not last in pcase")
      | lp ((m as A.PMatch(ps,e))::ms, acc) = let
          val bs = List.map bit ps
          in
	    lp (ms, bs::acc)
	  end
    in
      lp (ms, [])
    end

(* bitstringToString *)
(* compact 0 and 1 string representation of bitstrings *)
(* ex: bitstringToString [true, false, true] --> "101" *)
  fun bitstringToString (bs: bitstring) : string = 
    String.implode (List.map (fn b => if b then #"1" else #"0") bs)

(* bitstringCompare : bitstring -> order *)
(* used to build red-black set implementation (see below) *)
  fun bitstringCompare (bs1: bitstring, bs2: bitstring) : order = let
    fun lp ([], []) = EQUAL
      | lp ([], _::_) = LESS
      | lp (_::_, []) = GREATER
      | lp (b1::t1, b2::t2) =
          if b1 = b2 then lp (t1, t2)
          else if b1 then GREATER
	  else LESS
    in
      lp (bs1, bs2)
    end       

(* bitstringEq : bitstring * bitstring -> bool *)
  fun bitstringEq (bs1: bitstring, bs2: bitstring) : bool = 
    (bitstringCompare (bs1, bs2) = EQUAL)

(* bitstring sets *)
  structure BitstringSet = 
    RedBlackSetFn (struct
		     type ord_key = bitstring
		     val compare  = bitstringCompare
		   end)

(* bitstringSetFromList *)
  fun bitstringSetFromList (bss: bitstring list) : BitstringSet.set =
    List.foldl BitstringSet.add' BitstringSet.empty bss

(* bitstringToAST *)
(* Makes an AST expression out of an SML bitstring. *)
  local
    val new  = DV.bitvecNew ()
    val set1 = DV.bitvecSet1 ()
    val bvTy = DTy.bitvec ()
    fun seqList es =
      (case es
 	 of [] => raise Fail "undefined"
	  | [e] => e
	  | h::t => A.SeqExp (h, seqList t)
        (* end case *))
  in
    fun bitstringToAST (bs : bitstring) : A.exp = let
      val len = List.length bs
    (* BitVec.new constructs a bit string of length len, all zeros. *)
      val a = A.ApplyExp (A.VarExp (new, []),
			  ASTUtil.mkInt len,
			  bvTy)
      in
        if len <= 0 then a
        else let
          val aV = Var.new ("a", bvTy) 
	  fun set i = A.ApplyExp (A.VarExp (set1, []),
				  A.TupleExp [A.VarExp (aV, []),
					      ASTUtil.mkInt i],
				  B.unitTy)
	  fun lp (_, [], acc) = 
                A.LetExp (A.ValBind (A.VarPat aV, a),
		  	  seqList acc)
	    | lp (i, b::bs, acc) = let
                val acc' = if b then set(i)::acc else acc
                in
                  lp (i+1, bs, acc')
                end
          in
            lp (0, bs, [A.VarExp (aV, [])])
          end
        end	         
  end (* local *)

(* bitwiseAnd: bitwise and of two bitstrings. *)
(* pre: bitstring arguments are the same length. *)
  val bitwiseAnd : (bitstring * bitstring) -> bitstring = let
    fun andFn (b1, b2) = b1 andalso b2
    in
      ListPair.mapEq andFn
    end

(* next: set the kth bit to 1 *)
(* pre: i nonnegative *)
  fun next (k: int, bs: bitstring) : bitstring = let
    fun lp (i, b::bs) = 
          if i > 0 then b::lp(i-1,bs)
	  else if i = 0 then true::bs
	  else raise Fail ("not enough bits (" ^ Int.toString i ^ ")")
      | lp (_, []) = raise Fail "not enough bits (empty)"
    in
      if k < 0 then raise Fail "next: negative int argument"
      else lp (k, bs)
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
  (* The argument len is the total length of the bitstrings constructed. *)
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
(* pre: both arguments are the same length *)
(* ex: restrict([p1,p2,?,p4],1011) = [SOME p1, _, SOME p4] *)
  fun restrict (ps: AST.ppat list, bs: bitstring) : AST.pat list = let
    val optSOME = DV.optSOME ()
    fun lp ([], [], acc) = List.rev acc
      | lp (p::ps, false::bs, acc) = lp (ps, bs, acc)
      | lp (p::ps, true::bs, acc) = let
          val q = (case p 
		    of A.NDWildPat ty => A.WildPat (DTy.option ty)
		     | A.HandlePat _ => raise Fail "HandlePat not supported"
		     | A.Pat r => A.ConPat (optSOME, [TypeOf.pat r], r))
          in
            lp (ps, bs, q::acc)
	  end
      | lp _ = raise Fail "restrict: ps and bs different lengths"
    in
      lp (ps, bs, [])
    end

(* rules *)
  fun rules (s: bitstring, c: pcase') : (int * A.ppat list) list = let
    val PCase' (es, ms, bss, t) = c
    fun pred bs = bitstringEq (bs, bitwiseAnd (bs, s))
    fun lp (j, [], [], acc) = List.rev acc
      | lp (j, m::ms, bs::bss, acc) =
          if not (pred bs) then lp (j+1, ms, bss, acc)
	  else let
            val ps = 
              (case m
		 of A.PMatch (ps, _) => ps
		  | A.Otherwise _ => 
                      List.map (fn e => A.Pat (A.WildPat (TypeOf.exp e))) es
	        (* end case *))
            in
              lp (j+1, ms, bss, (j,ps)::acc)
            end
      | lp _ = raise Fail "inconsistent lengths (of ms and bss)"
    in
      lp (1, ms, bss, []) (* j is 1-based (see Fig. 17) *)
    end

(* avail *)
(* What it returns is actually an int set, unsafely implemented as a list. *)
  fun avail (s: bitstring) : int list = let
    fun lp ([], _, acc) = List.rev acc
      | lp (b::bs, k, acc) = lp (bs, k+1, if b then k::acc else acc)
    in
      lp (s, 0, [])
    end

  fun boundVars (ps: A.pat list) : Var.Set.set = let
    fun pat (AST.ConPat (_, _, p)) = pat p
      | pat (AST.TuplePat ps) = List.concat (List.map pat ps)
      | pat (AST.VarPat x) = [x]
      | pat (AST.WildPat _) = []
      | pat (AST.ConstPat _) = []
    fun addList' (x, s) = Var.Set.addList (s, x)
    in
      List.foldl addList' Var.Set.empty (List.map pat ps)
    end

  fun boundVarsP (ps: AST.ppat list) : Var.Set.set = let
    fun ppat (AST.NDWildPat _) = Var.Set.empty
      | ppat (AST.HandlePat _) = raise Fail "HandlePat not supported"
      | ppat (AST.Pat p) = boundVars [p]
    in
      List.foldl Var.Set.union Var.Set.empty (List.map ppat ps)
    end

(* don'tCare *)
  fun don'tCare (bs: bitstring) : int list = let
    fun lp ([], _, acc) = List.rev acc
      | lp (b::bs, k, acc) = lp (bs, k+1, if b then acc else k::acc)
    in
      lp (bs, 0, [])
    end

(* cancelThese *)
(* pre: cs and bs same length *)
  fun cancelThese (cs: A.var list, bs: bitstring) : A.var list = let
    fun lp ([], [], acc) = List.rev acc
      | lp (c::cs, true::bs, acc) = lp (cs, bs, acc)
      | lp (c::cs, false::bs, acc) = lp (cs, bs, c::acc)
      | lp _ = raise Fail "cs and bs not the same length"
    in
      lp (cs, bs, [])
    end


(* see JPF paper fig. 17 for this implementation *)

(* (2)
 * we need to generate this:
 *   val state = MVar.new S_0
 * S_0 is an array of falses. We need to know its length.
 *)
  fun initState (k: int) : A.binding * A.var = let
    val args = [ASTUtil.mkInt k, ASTUtil.falseExp]
    val newArr = A.VarExp (BasisUtil.arrayArray (), [Basis.boolTy])
    val s_0 = ASTUtil.mkApplyExp (newArr, args)
    in
      mvarNew ("state", s_0)
    end
  and mvarNew (name: string, init: A.exp) : A.binding * A.var = let
    val ty = DTy.mvar (TypeOf.exp init)
    val x = Var.new (name, ty)
    val new = A.VarExp (DV.mvarNew (), [Basis.boolTy])
    val rhs = ASTUtil.mkApplyExp (new, [init])
    in
      (A.ValBind (A.VarPat x, rhs), x)
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
  fun mkRefs (ts: A.ty list) : A.binding list * A.var list = let
    fun lp ([], j, accBs, accRs) = (accBs, accRs)
      | lp (t::ts, k, accBs, accRs) = let
          val (b, r) = mkRef (k, t)
          in
	    lp (ts, k-1, b::accBs, r::accRs)
	  end
    in
      lp (List.rev ts, List.length ts, [], [])
    end
  and mkRef (k: int, ty: A.ty) : A.binding * A.var = let
    val r = Var.new ("r" ^ Int.toString k, DTy.ref (DTy.option ty))
    val b = A.ValBind (A.VarPat r, refNONE ty)
    in
      (b, r)
    end
  and refNONE (ty: A.ty) : A.exp = let
    val refNew = A.VarExp (DV.refNew (), [DTy.option ty])
    val none = A.ConstExp (A.DConst (DD.optNONE (), [ty]))
    in
      ASTUtil.mkApplyExp (refNew, [none])
    end

(* (4) *)
  local
    val cancelTy = DTy.cancel ()
    val cancelNew = A.VarExp (DV.cancelNew (), [])
    val new = ASTUtil.mkApplyExp (cancelNew, [ASTUtil.unitExp])
  in
    fun mkCancels (k: int) : A.binding list  * A.var list = let
      fun lp (k, accBs, accCs) = 
        if k < 1 then (accBs, accCs)
        else let
          val c = Var.new ("c" ^ Int.toString k, cancelTy)
	  val b = A.ValBind (A.VarPat c, new)
          in
            lp (k-1, b::accBs, c::accCs)
          end
      in
        lp (k, [], [])
      end
  end (* local *)

(* (5) *)
  local
    val bitvecTy = DTy.bitvec ()
    val bitvecEq = DV.bitvecEq ()
    val bSet1F = DV.bitvecSet1F ()
    val refSetV = DV.refSet ()
    val optSOME = DD.optSOME ()
    val mTake = DV.mvarTake ()
    fun varExp x = A.VarExp (x, [])
  in
    fun mkTrans (k: int, argTy: A.ty, mState: A.var, r_k: A.var, 
		 stFuns: (bitstring * A.var) list, resTy: A.ty)
        : A.lambda = let
      val stV = Var.new ("st", bitvecTy)
      val v = Var.new ("v", argTy)
      val bind = A.ValBind (A.VarPat stV, 
			    A.ApplyExp (A.VarExp (mTake, [bitvecTy]), 
					varExp mState,
					bitvecTy))
      val someV = A.ApplyExp (A.ConstExp (A.DConst (optSOME, [argTy])),
			      varExp v,
			      DTy.option argTy)
      val setR = ASTUtil.mkApplyExp (A.VarExp (refSetV, [argTy]),
				     [varExp r_k, someV])
      val stV' = Var.new ("st'", bitvecTy)					 
      val bind' = A.ValBind (A.VarPat stV',
			     A.ApplyExp (varExp bSet1F, 
					 A.TupleExp [varExp stV, ASTUtil.mkInt k],
					 bitvecTy))
      fun mkTest bs = A.ApplyExp (A.VarExp (bitvecEq, []),
				  A.TupleExp [A.VarExp (stV', []),
					      bitstringToAST bs],
				  B.boolTy)
      fun mkApp stFun = 
        A.ApplyExp (A.VarExp (stFun, []), ASTUtil.unitExp, resTy)
    (* this loop makes a big nested if statetement *)
    (* the "default" is raise Match *)
      fun lp (ss, memNext_k) = let
        fun lp' [] = A.RaiseExp (A.ConstExp (A.DConst (B.exnMatch, [])), resTy)
	  | lp' ((bs,stFun)::t) = 
              if memNext_k bs 
	      then A.IfExp (mkTest bs, mkApp stFun, lp' t, resTy)	        
	      else lp' t
        in
          lp' ss
        end
      val cond =
        (case stFuns
           of [] => lp ([], fn _ => raise Fail "shouldn't be called")
	    | (bs,_)::_ => let
                 val next_k = nextSet (k, List.length bs)
		 fun memNext_k bs = List.exists ((curry bitstringEq) bs) next_k
                 in
                   lp (stFuns, memNext_k)
	         end
	  (* end case *))
      val body = A.LetExp (bind, A.SeqExp (setR, A.LetExp (bind', cond)))
      val trans_k = Var.new ("trans_" ^ Int.toString k, T.FunTy (argTy, resTy))
      in
        A.FB (trans_k, v, body)
      end
  end (* local *)

  fun lamName (A.FB (f, _, _)) = f

  fun allTrans (mState: A.var, 
		ers: (A.exp * A.var) list,
		stFuns: (bitstring * A.var) list,
	        resTy : A.ty) 
      : A.lambda list * A.var list = let
    fun lp (_, [], acc) = List.rev acc
      | lp (k, (e,r)::t, acc) = let
          val trans_k = mkTrans (k, TypeOf.exp e, mState, r, stFuns, resTy)
          in
            lp (k+1, t, trans_k::acc)
	  end
    val lams = lp (1, ers, [])
    in
      (lams, List.map lamName lams)
    end

(* (6) *)
  local
    fun varType x = TypeUtil.toMonoTy (Var.typeOf x)
    val refTyc = DT.ref ()
    fun typeOfRefVar (r: A.var) : A.ty =
      (case TypeUtil.toMonoTy (Var.typeOf r)
         of ty as A.ConTy ([t], c) =>
              if TyCon.same (c, refTyc)
	      then t
	      else raise Fail ("expected ref type, got " ^ TypeUtil.toString ty)
	  | ty => raise Fail ("expected ref type, got " ^ TypeUtil.toString ty)
        (* end case *))
    fun bang (r: A.var) : A.exp = let
      val t = typeOfRefVar r
      in
	A.ApplyExp (A.VarExp (DV.refGet (), [t]), A.VarExp (r, []), t)
      end			     
    fun bigBang (rs: A.var list) : A.exp =
      (case rs
	 of [] => ASTUtil.unitExp
	  | _  => A.TupleExp (List.map bang rs)
        (* end case *))            
    fun pats (ps: A.ppat list) : A.pat = let
      fun ppat (A.NDWildPat ty) = A.WildPat ty
	| ppat (A.HandlePat _) = raise Fail "not supported: HandlePat"
	| ppat (A.Pat p) = p
      in
        A.TuplePat (List.map ppat ps)
      end
  in 
    fun mkState (s_i: bitstring, rs: A.var list, acts: A.var list, resume: A.var, c: pcase') 
      : A.lambda = let
      val (PCase' (_, _, _, ty)) = c
      fun rsLp ([], [], acc) = bigBang (List.rev acc)
	| rsLp (r::rs, true::bs, acc) = rsLp (rs, bs, r::acc)
	| rsLp (r::rs, false::bs, acc) = rsLp (rs, bs, acc)
	| rsLp _ = raise Fail "rs and bs must be the same length"
      fun lp ([], m::acc) = let
            val w =
              (case m 
		 of A.PatMatch (p, _) => A.WildPat (TypeOf.pat p)
		  | A.CondMatch (p, _, _) => A.WildPat (TypeOf.pat p)
	        (* end case *))
	    val applyResume = A.ApplyExp (A.VarExp (resume, []), bitstringToAST s_i, ty)
            in 
              List.rev (A.PatMatch(w,applyResume)::acc)
            end
	| lp ([], []) = raise Fail "no rules at all? shouldn't happen."
	| lp ((j,ps)::t, acc) = let
            val xs = Var.Set.listItems (boundVarsP ps)
	    val arg = A.TupleExp (List.map (fn x => A.VarExp (x, [])) xs) 
	    val act_j = List.nth (acts, j-1) (* j is 1-based (see Fig. 17), nth is 0-based *)
	    val app = A.ApplyExp (A.VarExp (act_j, []), arg, ty)
            val m = A.PatMatch (pats ps, app)
            in
              lp (t, m::acc)
	    end
      val e = rsLp (rs, s_i, [])
      val ms = lp (rules (s_i, c), [])
      val fnName = "state_" ^ bitstringToString s_i
      val body = A.CaseExp (e, ms, ty)
      in
        A.FB (Var.new (fnName, T.FunTy (B.unitTy, ty)),
	      Var.new ("u", B.unitTy),
	      body)
      end
  (* mkStates : pcase' * A.var list * A.var list * A.var -> A.lambda list * A.var list *)
    fun mkStates (c as PCase' (es, ms, bss, ty), rs: A.var list, acts: A.var list, resume: A.var) 
      : bitstring list * A.lambda list * A.var list = let
      val ss = BitstringSet.listItems (bitstringSetFromList bss)
      fun mk s_i = mkState (s_i, rs, acts, resume, c)
      val lams = List.map mk ss
      in
        (ss, lams, List.map lamName lams)
      end
  end (* local *)

(* (7) *)
  fun mkActs (pc: pcase', returnV: A.var, reraiseV: A.var, cs: A.var list) 
      : A.lambda list * A.var list = let
    fun varType x = TypeUtil.toMonoTy (Var.typeOf x)
    val (PCase' (es, ms, bss, ty)) = pc
    val len = List.length ms
    fun newUnitV () = Var.new ("u", Basis.unitTy)
    fun newExV () = Var.new ("ex", Basis.exnTy)
    fun mkReraise exV = 
      ASTUtil.mkApplyExp (AST.VarExp (reraiseV, [T.VarTy (TyVar.new (Atom.atom "'e"))]),
			  [AST.VarExp (exV, [])])
    fun mkHandle (e, exV, rer) = A.HandleExp (e, [A.PatMatch (A.VarPat exV, rer)], ty)
    fun mkLam (actV, arg, cs, e) = let
      val exV = newExV ()
      val rer = mkReraise exV
      val h = mkHandle (e, exV, rer)
      val b = AST.ApplyExp (AST.VarExp (returnV, []), h, ty)
      val b' = List.foldr (fn (c, e) => A.SeqExp (cancel c, e)) b cs
      in
        A.FB (actV, arg, b')
      end      
    fun lp (_, [], _, acc) = List.rev acc
      | lp (j, A.Otherwise(e)::ms, bs::bss, acc) = raise Fail "FIXME" (* otherwise has changed *)
          (* (case ms  *)
	  (*    of [] => let *)
          (*         val act_j = Var.new ("act" ^ Int.toString j,  *)
	  (* 			       T.FunTy (B.unitTy, ty)) *)
	  (* 	  val lam = mkLam (act_j, Var.new ("u", B.unitTy), [], e) *)
	  (* 	  in *)
	  (* 	    lp (j+1, [], bss, lam::acc) *)
	  (* 	  end *)
	  (*     | _ => raise Fail "otherwise is not last" *)
	  (*   (\* end case *\)) *)
      | lp (j, A.PMatch (ps, e)::ms, bs::bss, acc) = let
          val ps' = restrict (ps, bs)
          val xs = Var.Set.listItems (boundVars ps')
          val ys = List.map (fn v => Var.new (Var.nameOf v, varType v)) xs
	  val env = ListPair.foldl VarSubst.add' VarSubst.id (xs, ys)
          val (argV, argTy, e') = 
            (case List.map varType xs
	       of [] => (Var.new ("u", B.unitTy), B.unitTy, e)
		| [t] => (Var.new ("arg", t), t, VarSubst.applySubst env e)
		| ts => let
		    val ty = T.TupleTy ts
		    val argV = Var.new ("arg", ty)
                    val bind = A.ValBind (A.TuplePat (List.map A.VarPat ys),
					  A.VarExp (argV, []))
                    val e' = A.LetExp (bind, VarSubst.applySubst env e)
		    in
                      (argV, ty, e')
                    end
	      (* end case *))
          val act_j = Var.new ("act" ^ Int.toString j, T.FunTy (argTy, ty))
	  val lam = mkLam (act_j, argV, cancelThese (cs, bs), e')
          in
	    lp (j+1, [], bss, lam::acc)
          end
      | lp _ = raise Fail "ms and bss are supposed to be the same length"
    val lams = lp (1, ms, bss, [])
    in
      (lams, List.map lamName lams)
    end

(* (8) *)
  fun mkReraise (exnReturnV: A.var, cs: A.var list, ty: A.ty) : A.lambda * A.var = let
    val reraiseV = Var.new ("reraise", T.FunTy (B.exnTy, ty))
    val x = Var.new ("x", B.exnTy)
    val etys = [T.VarTy (TyVar.new (Atom.atom "'e"))]
    val exnReturnX = 
      A.ApplyExp (A.VarExp (exnReturnV, etys), A.VarExp (x, []), ty)
    val body = 
      List.foldr (fn (c,b) => A.SeqExp (cancel c, b)) exnReturnX cs 
    in
      (A.FB (reraiseV, x, body), reraiseV)
    end

(* mkActExn : A.var * A.var * A.ty -> A.lambda * A.var *)
  local
    val bitvecTy = DTy.bitvec ()
    val mvarTake = DV.mvarTake ()
  in
    fun mkActExn (stV: A.var, reraiseV: A.var, resTy: A.ty) : A.lambda * A.var = let
      val actExnV = Var.new ("actExn", T.FunTy (B.exnTy, resTy))
      val exV = Var.new ("ex", B.exnTy)
      val take = A.ApplyExp (A.VarExp (mvarTake, [bitvecTy]),
			     A.VarExp (stV, []),
			     bitvecTy)
      val rer = A.ApplyExp (A.VarExp (reraiseV, []),
			    A.VarExp (exV, []),
			    resTy)
      val lam = A.FB (actExnV, exV, A.SeqExp (take, rer))
      in
        (lam, actExnV)
      end
  end

(* (9) *)
(* cs = list of cancel vars  *)
(* ts = list of trans_k vars *)
(* es = list of expressions  *)
  fun mkBody (cs: A.var list, ts: A.var list, es: A.exp list, 
	      resTy: T.ty, dispatchCall: A.exp, actExnV: A.var) 
      : A.exp = let
    fun lp ([], [], [], b) = b
      | lp (c::cs, t::ts, e::es, b) = let
          val eTy = TypeOf.exp e
          val exV = Var.new ("ex", Basis.exnTy)
          val m = AST.PatMatch (AST.VarPat exV,
				AST.ApplyExp (AST.VarExp (actExnV, []),
					      AST.VarExp (exV, []),
					      eTy))
          val h = AST.HandleExp (e, [m], TypeOf.exp e)
          val f = thunk (AST.ApplyExp (AST.VarExp (t, []), h, resTy))
          val s = spawn (c, f)
          in
            A.SeqExp (s, lp (cs, ts, es, b))
          end
      | lp _ = raise Fail "precondition: cs, ts, es should all be the same length"
    in
      lp (cs, ts, es, dispatchCall)
    end

(* all together now *)
  local
    val bitvecTy = DTy.bitvec ()
    val mPut = DV.mvarPut ()
    fun mvarPut (x: A.var, v: A.exp) : A.exp = let
      val t = TypeOf.exp v
      in
        A.ApplyExp (A.VarExp (x, [t]), v, DTy.mvar t)
      end
    fun pcase (es: A.exp list, ms: A.pmatch list, ty: A.ty) : A.exp = let
      val dispatchResTy = freshAlpha ()
      val returnTy = T.FunTy (ty, freshAlpha ())
      val exnReturnTy = T.FunTy (Basis.exnTy, freshAlpha ())
      val pcaseWrapperArgTy = T.FunTy (T.TupleTy [returnTy, exnReturnTy], freshAlpha ())
      val pcaseWrapperResTy = ty
      val dispatchV = (* FIXME *) raise Fail "?"
      val pcaseWrapperV = (* FIXME *) raise Fail "?"
      val dispatchCall = A.ApplyExp (A.VarExp (dispatchV, []), ASTUtil.unitExp, dispatchResTy)
      val c' = mkBitstrings (es, ms, ty)
      val len = List.length es
      val returnV = Var.new ("return", returnTy)
      val exnReturnV = Var.new ("exnReturn", exnReturnTy)
      val (stateBind, stateV) = initState len (* (2) *)
      val resumeV = Var.new ("resume", T.FunTy (B.unitTy, dispatchResTy))
      val stV = Var.new ("st", bitvecTy)
      val resumeBody = A.SeqExp (mvarPut (stateV, A.VarExp (stV, [])), dispatchCall)
      val resumeLam = A.FB (resumeV, stV, resumeBody)
      val (refBinds, refVs) = mkRefs (List.map (DTy.ref o DTy.option o TypeOf.exp) es) (* (3) *)
      val (cBinds, cVs) = mkCancels len (* (4) *)
      val (reraiseLam, reraiseV) = mkReraise (exnReturnV, cVs, ty)    (* (8) *)
      val (actLams, actVs) = mkActs (c', returnV, reraiseV, cVs)  (* (7) *)
      val (stateBits, stateLams, stateVs) = mkStates (c', refVs, actVs, resumeV) (* (6) *)
      val (transLams, transVs) = let
        val ers = ListPair.zipEq (es, refVs)
        val stFuns = ListPair.zipEq (stateBits, stateVs)
        in
          allTrans (stateV, ers, stFuns, ty)
        end
      val (actExnLam, actExnV) = mkActExn (stV, reraiseV, ty)
      val spawnAndDispatch = mkBody (cVs, transVs, es, ty, dispatchCall, actExnV) (* (9) *)
      val funArgV = Var.new ("arg", T.TupleTy [returnTy, exnReturnTy])
      val deconstructArg = A.ValBind (A.TuplePat [A.VarPat returnV, A.VarPat exnReturnV],
				      A.VarExp (funArgV, []))
      val funBody = 
        A.LetExp (deconstructArg,
        A.LetExp (stateBind,
	A.LetExp (A.FunBind [resumeLam],
        A.LetExp (A.FunBind actLams,
        A.LetExp (A.FunBind stateLams,
        A.LetExp (A.FunBind transLams,
        A.LetExp (A.FunBind [reraiseLam],
        A.LetExp (A.FunBind [actExnLam],
          spawnAndDispatch))))))))
      val funBody' = List.foldr A.LetExp funBody (refBinds@cBinds)
      in
        A.ApplyExp (A.VarExp (pcaseWrapperV, []),
		    A.FunExp (funArgV, funBody', pcaseWrapperArgTy),
                    pcaseWrapperResTy)
      end
    fun exp e =
      (case e
         of A.LetExp (b, e) => A.LetExp (binding b, exp e)
	  | A.IfExp (e1, e2, e3, t) => 
              A.IfExp (exp e1, exp e2, exp e3, t)
	  | A.CaseExp (e, ms, t) => 
	      A.CaseExp (exp e, List.map match ms, t)
	  | A.PCaseExp (es, ms, t) =>
              pcase (List.map exp es, List.map pmatch ms, t)
	  | A.HandleExp (e, ms, t) => 
	      A.HandleExp (exp e, List.map match ms, t)
	  | A.RaiseExp (e, t) => A.RaiseExp (exp e, t)
	  | A.FunExp (x, e, t) => A.FunExp (x, exp e, t)
	  | A.ApplyExp (e1, e2, t) => A.ApplyExp (exp e1, exp e2, t)
	  | oper as A.VarArityOpExp _ => oper
	  | A.TupleExp es => A.TupleExp (List.map exp es)
	  | A.RangeExp (e1, e2, optE, t) =>
              A.RangeExp (exp e1, exp e2, Option.map exp optE, t)
	  | A.PTupleExp es => A.PTupleExp (List.map exp es)
	  | A.PArrayExp (es, t) => A.PArrayExp (List.map exp es, t)
	  | A.PCompExp (e, pes, optE) => 
              A.PCompExp (exp e,
			  List.map (fn (p,e) => (p, exp e)) pes, 
			  Option.map exp optE)
	  | A.PChoiceExp (es, t) => A.PChoiceExp (List.map exp es, t)
	  | A.SpawnExp e => A.SpawnExp (exp e)
	  | k as A.ConstExp _ => k
	  | x as A.VarExp _ => x
	  | A.SeqExp (e1, e2) => A.SeqExp (exp e1, exp e2)
	  | ov as A.OverloadExp _ => ov
	  | A.ExpansionOptsExp (opts, e) => A.ExpansionOptsExp (opts, exp e)
        (* end case *))
    and binding b = 
      (case b
         of A.ValBind (p, e) => A.ValBind (p, exp e)
	  | A.PValBind (p, e) => A.PValBind (p, exp e)
	  | A.FunBind lams => A.FunBind (List.map lambda lams)
	  | p as A.PrimVBind _ => p
	  | p as A.PrimCodeBind _ => p
        (* end case *))
    and match m = 
      (case m
	of A.PatMatch (p, e) => A.PatMatch (p, exp e)
	 | A.CondMatch (p, e1, e2) => A.CondMatch (p, exp e1, exp e2)
       (* end case *))
    and pmatch m = 
      (case m
         of A.PMatch (ps, e) => A.PMatch (ps, exp e)
	  | A.Otherwise (ts, e) => A.Otherwise (ts, exp e)
        (* end case *))
    and lambda (A.FB (f, x, e)) = A.FB (f, x, exp e)
    in
      fun translate (e: A.exp) : A.exp = exp e
    end (* local *)

end
    


(* parray-op-gen.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate functions, as AST, from parray_op values.
 *
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure PArrayOpGen = struct

  structure A = AST
  structure B = Basis
  structure T = Types
  structure P = PArrayOp

  structure AU = ASTUtil
  structure TU = TypeUtil

  fun mapi f xs = let
    fun m (_, [], acc) = List.rev acc
      | m (i, x::xs, acc) = m (i+1, xs, f(x,i)::acc)
    in
      m (0, xs, [])
    end

  fun memo get path = let
    val cell = ref NONE
    fun remember x = (cell := SOME x; x)
    fun read () = (case !cell
      of SOME x => x
       | NONE => remember (get path)
      (* end case *))
    in
      read
    end

  val memoTyc = memo BasisEnv.getTyConFromBasis
  val memoVar = memo BasisEnv.getVarFromBasis

  val farrayTyc    = memoTyc ["FArray", "f_array"]
  val flatSub      = memoVar ["FArray", "flatSub"]
  val nestedSub    = memoVar ["FArray", "nestedSub"]
  val flen         = memoVar ["FArray", "length"]
  val ftab         = memoVar ["FArray", "tab"]
  val flatMap      = memoVar ["FArray", "flatMap"]
  val flatMap2     = memoVar ["FArrayPair", "flatMapEq"]
  val groundReduce = memoVar ["FArray", "groundReduce"]
  val intRange     = memoVar ["FArray", "intRange"]

  local
    fun isg c = List.exists (fn g => TyCon.same (c,g)) B.primTycs
  in
    fun isGroundTy (T.ConTy ([], c)) = isg c
      | isGroundTy _ = false
  end (* local *)

  fun isFArrayTyc c = TyCon.same (c, farrayTyc ())

  fun mkHash1 tys = (case tys
    of [] => raise Fail "mkHash1: nil"
     | t::ts => let
         val domTy = T.TupleTy tys
	 val rngTy = t
	 val arg = Var.new ("arg", domTy)
	 val x = Var.new ("x", rngTy)
	 val patVars = (A.VarPat x) :: (List.map A.WildPat ts)
	 val pat = A.TuplePat patVars
	 val body = A.LetExp (A.ValBind (pat, A.VarExp (arg, [])),
			      A.VarExp (x, []))
         in
	   A.FunExp (arg, body, rngTy)
         end
    (* end case *))

  fun genLength (ty : T.ty) : A.exp = let
    val ty' = TU.prune ty
    in case ty'
      of T.TupleTy (ts as t::_) => let
           val len' = genLength t
	   val hash1 = mkHash1 ts
	   val c = AU.mkCompose (len', hash1)
           in
             c
           end 
       | T.ConTy (ts, c) => 
           if isFArrayTyc c then
             A.VarExp (flen (), ts)
	   else
	     raise Fail ("gen: unexpected type (not farray) " ^ TU.toString ty')
       | _ => raise Fail ("gen: unexpected type " ^ TU.toString ty')
    end

  fun genSub s = let
    fun g (A.PSub_Nested t) = (case t 
          of T.ConTy ([t'], c) =>
               if isFArrayTyc c
	         then A.VarExp (nestedSub(), [t'])
	         else raise Fail ("unexpected ConTy " ^ TU.toString t)
	   | _ => raise Fail ("unexpected ty " ^ TU.toString t)
	  (* end case *))
      | g (A.PSub_Flat t) = (case t 
          of T.ConTy ([t'], c) =>
               if isFArrayTyc c
	         then A.VarExp (flatSub(), [t'])
	         else raise Fail ("unexpected ConTy " ^ TU.toString t)
	   | _ => raise Fail ("unexpected ty " ^ TU.toString t)
	  (* end case *))
      | g (A.PSub_Tuple ss) = let
          val opers = List.map g ss
	  val ts = List.map TypeOf.exp opers
	  val ds = List.map TU.domainType ts
	  fun firstTyOfPair (T.TupleTy [t1, _]) = t1
	    | firstTyOfPair t = raise Fail ("unexpected type " ^ TU.toString t)
	  val fs = List.map firstTyOfPair ds
	  val arg = Var.new ("arg", T.TupleTy [T.TupleTy fs, B.intTy])
	  val tup = Var.new ("tup", T.TupleTy fs)
	  val i = Var.new ("i", B.intTy)
 	  fun mkX (t, i) = Var.new ("x_" ^ Int.toString i, t)
	  val xs = mapi mkX fs
	  fun mkApp (oper, x) = let
            val arg = A.TupleExp [A.VarExp (x, []), A.VarExp (i, [])]
            in
	      AU.mkApplyExp (oper, [arg])
	    end
	  val apps = ListPair.map mkApp (opers, xs)
	  val bind1 = A.ValBind (A.TuplePat [A.VarPat tup, A.VarPat i],
				 A.VarExp (arg, []))
	  val bind2 = A.ValBind (A.TuplePat (List.map A.VarPat xs),
				 A.VarExp (tup, []))
	  val body = AU.mkLetExp ([bind1, bind2], A.TupleExp apps)
          in
	    A.FunExp (arg, body, TypeOf.exp body)
	  end
    in
      g s
    end

  fun genTab t = A.VarExp (ftab (), [t])
          
  fun genMap (t as T.FunTy (alpha, beta)) = 
        if isGroundTy alpha then
          A.VarExp (flatMap (), [alpha, beta])
	else (case alpha
          of tup as T.TupleTy [t1, t2] =>
               if isGroundTy t1 andalso isGroundTy t2 then
	         A.VarExp (flatMap2 (), [alpha, beta])
	       else raise Fail ("todo: " ^ TU.toString tup)
	   | _ => raise Fail ("todo: " ^ TU.toString t)
          (* end case *))
    | genMap t = raise Fail ("unexpected ty " ^ TU.toString t)

  fun genReduce t =
    if isGroundTy t then
      A.VarExp (groundReduce (), [t])
    else
      raise Fail ("todo: reduce for type " ^ TU.toString t)

  fun genRange t =
    if TU.same (t, B.intTy) then
      A.VarExp (intRange (), [])
    else
      raise Fail ("unexpected type " ^ TU.toString t)

  fun gen (pop : A.parray_op) : A.exp = (case pop
    of A.PA_Length ty => genLength ty
     | A.PA_Sub s => genSub s		     
     | A.PA_Tab t => genTab t
     | A.PA_Map t => genMap t
     | A.PA_Reduce t => genReduce t
     | A.PA_Range t => genRange t
    (* end case *))

end

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

  structure D  = DelayedBasis
  structure DC = D.TyCon
  structure DD = D.DataCon
  structure DV = D.Var

  structure AU = ASTUtil
  structure TU = TypeUtil
  structure FU = FlattenUtil

  fun println s = (print s; print "\n")

  fun both pred (x, y) = pred x andalso pred y

  fun mapi f xs = let
    fun m (_, [], acc) = List.rev acc
      | m (i, x::xs, acc) = m (i+1, xs, f(x,i)::acc)
    in
      m (0, xs, [])
    end
		
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

  fun monoVarExp v = A.VarExp (v, [])
  fun monoVarExp' thunk = A.VarExp (thunk (), [])

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
       | T.ConTy (ts, c) => (case ts
           of [] (* monomorphic *) =>
                if TyCon.same (c, DC.int_farray ()) then
                  monoVarExp' DV.intLen
		else if TyCon.same (c, DC.dbl_farray ()) then
                  monoVarExp' DV.dblLen
		else
                  raise Fail ("genLength: unexpected type " ^ TU.toString ty')
	    | _ (* t farray *) =>
                if FU.isFArrayTyc c then
                  A.VarExp (DV.flen (), ts)
		else
	          raise Fail ("gen: unexpected type (not farray) " ^ TU.toString ty')
           (* end case *))
       | _ => raise Fail ("gen: unexpected type " ^ TU.toString ty')
    end

  fun genSub s = let
    fun g (A.PSub_Nested t) = 
            if FU.isInt_farray t then
              monoVarExp' DV.intNestedSub
	    else if FU.isDbl_farray t then
              monoVarExp' DV.dblNestedSub
	    else (case t 
              of T.ConTy ([t'], c) =>
                   if FU.isFArrayTyc c then 
                     A.VarExp (DV.nestedSub (), [t'])
	           else 
	             raise Fail ("genSub.PSub_Nested: unexpected ConTy " ^ 
				 TU.toString t)
	       | _ => raise Fail ("genSub.PSub_Nested: unexpected ty " ^ 
				  TU.toString t)
	      (* end case *))
      | g (A.PSub_Flat t) = (case t 
          of T.ConTy ([t'], c) =>
               if FU.isFArrayTyc c then 
	         A.VarExp (DV.flatSub (), [t'])
	       else 
		 raise Fail ("unexpected ConTy " ^ TU.toString t)
	   | T.ConTy ([], c) =>
               if FU.isInt_farray t then
	         monoVarExp' DV.intFlatSub
	       else if FU.isDbl_farray t then
                 monoVarExp' DV.dblFlatSub
	       else
	         raise Fail ("unexpected ty " ^ TU.toString t)
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
            val arg = A.TupleExp [monoVarExp x, monoVarExp i]
            in
	      AU.mkApplyExp (oper, [arg])
	    end
	  val apps = ListPair.map mkApp (opers, xs)
	  val bind1 = A.ValBind (A.TuplePat [A.VarPat tup, A.VarPat i],
				 monoVarExp arg)
	  val bind2 = A.ValBind (A.TuplePat (List.map A.VarPat xs),
				 monoVarExp tup)
	  val body = AU.mkLetExp ([bind1, bind2], A.TupleExp apps)
          in
	    A.FunExp (arg, body, TypeOf.exp body)
	  end
    in
      g s
    end

  fun genTab (t : T.ty) : A.exp =
    if FU.isInt t then 
      monoVarExp' DV.intTab
    else if FU.isDouble t then
      monoVarExp' DV.dblTab
    else (case t
      of T.TupleTy [t1, t2] => 
           if FU.isInt t1 andalso FU.isInt t2 then
             A.VarExp (DV.ifpTab (), [])
	   else
             A.VarExp (DV.ftab (), [t])
       | _ => A.VarExp (DV.ftab (), [t])
      (* end case *))

  fun genTabFTS (t : T.ty) : A.exp = 
    if FU.isInt t then
      monoVarExp' DV.intTabFTS
    else if FU.isDouble t then
      monoVarExp' DV.dblTabFTS
    else
      A.VarExp (DV.ftabFTS (), [t])

  fun genTabTupleFTS ts = (case ts
    of [t1, t2] => 
         if FU.isInt t1 andalso FU.isInt t2 then
           A.VarExp (DV.ifpTabFTS (), [])
(* FIXME What if only one of the types is an int? *)
	 else
           A.VarExp (DV.fptabFTS (), ts)
     | _ => raise Fail ("todo: " ^ TU.toString (T.TupleTy ts))
    (* end case *))
          
  fun genMap (t as T.FunTy (alpha, beta)) = 
(* +debug *)
(print ("genMap called with " ^ TU.toString t ^ "\n");
(* -debug *)
        if both FU.isInt (alpha, beta) then
          monoVarExp' DV.intMap
	else if both FU.isDouble (alpha, beta) then
          monoVarExp' DV.dblMap
	else if FU.isInt alpha then let
          val e = A.VarExp (DV.mapIFPoly (), [beta])
(* +debug *)
val _ = print ("genMap made mapIFPoly at type " ^ TU.toString t ^ "\n")
val _ = print ("-- its type is " ^ TU.toString (TypeOf.exp e) ^ "\n")
(* -debug *)
          in
            e
          end
        else if FU.isGroundTy alpha then
          A.VarExp (DV.fmap (), [alpha, beta])
	else (case alpha
          of tup as T.TupleTy [t1, t2] =>
               if both FU.isGroundTy (t1, t2) then
	         if List.all FU.isInt [t1, t2, beta] then
                   monoVarExp' DV.ipMapEq_int
		 else 
		   A.VarExp (DV.fpmap (), [alpha, beta])
	       else raise Fail ("genMap(loc1) todo: " ^ TU.toString tup)
	   | _ => raise Fail ("genMap(loc2) todo: " ^ TU.toString t)
          (* end case *))
)
    | genMap t = raise Fail ("unexpected ty " ^ TU.toString t)

  local
    fun isIntPair (T.TupleTy [t1, t2]) = both FU.isInt (t1, t2)
      | isIntPair _ = false
  in
    fun genReduce (t : T.ty) : A.exp =
      if FU.isInt t then
        monoVarExp' DV.intReduce
      else if FU.isDouble t then
        monoVarExp' DV.dblReduce
      else if isIntPair t then
	monoVarExp' DV.ipReduce
      else if FU.isGroundTy t then
        A.VarExp (DV.greduce (), [t])
      else 
        raise Fail ("todo: reduce for type " ^ TU.toString t)
  end

  fun genRange t =
    if FU.isInt t then
      A.VarExp (DV.intRange (), [])
    else
      raise Fail ("unexpected type " ^ TU.toString t)

  fun genApp t = 
    if FU.isGroundTy t then
      A.VarExp (DV.fapp (), [t])
    else
      raise Fail ("todo: app for type " ^ TU.toString t)

  fun gen (pop : A.parray_op) : A.exp = let
(*  val _ = println ("*** generating " ^ PArrayOp.toString pop)  *)
    val g = (case pop
      of A.PA_Length ty => genLength ty
       | A.PA_Sub s => genSub s	
       | A.PA_Tab t => genTab t	     
       | A.PA_TabFTS t => genTabFTS t
       | A.PA_TabTupleFTS ts => genTabTupleFTS ts
       | A.PA_Map t => genMap t
       | A.PA_Reduce t => genReduce t
       | A.PA_Range t => genRange t
       | A.PA_App t => genApp t
      (* end case *))
    in
(*    PrintAST.printExp g; *)
      g
    end

end

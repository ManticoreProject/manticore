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
       | T.ConTy ([], c) =>
           if TyCon.same (c, DC.intFArray ()) then let
             val _ = () (* println ("inserting intLength") *)
             in
               A.VarExp (DV.intLen (), [])
	     end
           else
             raise Fail ("genLength: unexpected type " ^ TU.toString ty')
       | T.ConTy (ts, c) => 
           if FU.isFArrayTyc c then
             A.VarExp (DV.flen (), ts)
	   else
	     raise Fail ("gen: unexpected type (not farray) " ^ TU.toString ty')
       | _ => raise Fail ("gen: unexpected type " ^ TU.toString ty')
    end

  fun genSub s = let
    fun g (A.PSub_Nested t) = (case t 
          of T.ConTy ([t'], c) =>
               if FU.isFArrayTyc c
	         then A.VarExp (DV.nestedSub (), [t'])
	         else raise Fail ("unexpected ConTy " ^ TU.toString t)
	   | _ => raise Fail ("unexpected ty " ^ TU.toString t)
	  (* end case *))
      | g (A.PSub_Flat t) = (case t 
          of T.ConTy ([t'], c) =>
               if FU.isFArrayTyc c then 
	         A.VarExp (DV.flatSub (), [t'])
	       else 
		 raise Fail ("unexpected ConTy " ^ TU.toString t)
	   | T.ConTy ([], c) =>
               if TyCon.same (c, DC.intFArray ()) then let
                 val _ = () (* println ("inserting intFlatSub") *)
                 in
		   A.VarExp (DV.intFlatSub (), [])
		 end
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

  fun genTab (t : T.ty) : A.exp =
    if TU.same (t, B.intTy) then let
      val _ = () (* println ("inserting intTab") *)
      in      
        A.VarExp (DV.intTab (), [])
      end
    else
      A.VarExp (DV.ftab (), [t])

  fun genTabFTS (t : T.ty) : A.exp = 
    if FU.isInt t then
      A.VarExp (DV.intTabFTS (), [])
    else
      A.VarExp (DV.ftabFTS (), [t])

  fun genTabTupleFTS ts = (case ts
    of [t1, t2] => 
         if FU.isInt t1 andalso FU.isInt t2 then
           A.VarExp (DV.ifpTabFTS (), [])
(* FIXME What is only one of the types is an int? *)
	 else
           A.VarExp (DV.fptabFTS (), ts)
     | _ => raise Fail ("todo: " ^ TU.toString (T.TupleTy ts))
    (* end case *))
          
  fun genMap (t as T.FunTy (alpha, beta)) = 
        if FU.isInt alpha andalso FU.isInt beta then
          A.VarExp (DV.ifmap (), [])
        else if FU.isGroundTy alpha then
          A.VarExp (DV.fmap (), [alpha, beta])
	else (case alpha
          of tup as T.TupleTy [t1, t2] =>
               if FU.isGroundTy t1 andalso FU.isGroundTy t2 then
	         if List.all FU.isInt [t1, t2, beta] then
                   A.VarExp (DV.ipMapEq_int (), [])
		 else 
		   A.VarExp (DV.fpmap (), [alpha, beta])
	       else raise Fail ("genMap(loc1) todo: " ^ TU.toString tup)
	   | _ => raise Fail ("genMap(loc2) todo: " ^ TU.toString t)
          (* end case *))
    | genMap t = raise Fail ("unexpected ty " ^ TU.toString t)

  local
    fun isIntPair (T.TupleTy [t1, t2]) = FU.isInt t1 andalso FU.isInt t2
      | isIntPair _ = false
  in
    fun genReduce (t : T.ty) : A.exp =
      if FU.isInt t then
        A.VarExp (DV.ifReduce (), [])
      else if isIntPair t then
	A.VarExp (DV.ipReduce (), [])
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

  fun gen (pop : A.parray_op) : A.exp = (case pop
    of A.PA_Length ty => genLength ty
     | A.PA_Sub s => genSub s	
     | A.PA_Tab t => genTab t	     
     | A.PA_TabFromToStep t => genTabFTS t
     | A.PA_TabTupleFTS ts => genTabTupleFTS ts
     | A.PA_Map t => genMap t
     | A.PA_Reduce t => genReduce t
     | A.PA_Range t => genRange t
     | A.PA_App t => genApp t
    (* end case *))

end

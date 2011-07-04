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
  structure DTy = D.Ty

  structure AU = ASTUtil
  structure TU = TypeUtil
  structure FU = FlattenUtil

  structure M = SynthMap

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
           if FU.isGroundTy t1 andalso FU.isGroundTy t2 then let 
	     val _ = print (concat ["*** GENERATING TAB FOR ", 
				    TU.toString t1, "*", TU.toString t2, "\n"])
             val {seqPair, tabFromToP, tabP, fTab} = SynthTab.mkFTab (t1, t2)
	     val A.FB (f, _, _) = fTab
	     fun b lam = A.FunBind [lam]
	     val binds = List.map b [seqPair, tabFromToP, tabP, fTab]
             in
	       AU.mkLetExp (binds, monoVarExp f)
	     end
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

  (* fun genTab2D (t : T.ty) : A.exp = (case t *)
  (*   of T.TupleTy _ => raise Fail "todo: 2D tabs for tuple tys" *)
  (*    | _ => let *)
  (*        val tab2DLam = SynthTab.mkTab2D t *)
  (*        val A.FB (tab2D, _, _) = tab2DLam *)
  (*        in *)
  (*          AU.mkLetExp ([A.FunBind [tab2DLam]], A.VarExp (tab2D, [])) *)
  (*        end *)
  (*   (\* end case *\))   *)

  fun genTabHD (dim : int, eltTy : T.ty) : A.exp = (case eltTy
    of T.TupleTy _ => raise Fail "todo: higher D tabs of tuples"
     | _ => let
         val tabHDLam = SynthTab.mkTabHD (dim, eltTy)
         val A.FB (tabHD, _, _) = tabHDLam
         in
	   AU.mkLetExp ([A.FunBind [tabHDLam]], A.VarExp (tabHD, []))
         end
    (* end case *))

  fun genTabTupleFTS ts = (case ts
    of [t1, t2] => 
         if FU.isGroundTy t1 andalso FU.isGroundTy t2 then let
           val {seqPair=f1, tabFromToP=f2, tabFromToStepP=f3, fTabFromToStep=f4} =
             SynthTab.mkFTabFTS (t1, t2)
	   val A.FB (f, _, _) = f4
	   val binds = List.map (fn lam => A.FunBind [lam]) [f1, f2, f3, f4]
	   in
             AU.mkLetExp (binds, monoVarExp f)
	   end
	 else
           A.VarExp (DV.fptabFTS (), ts)
     | _ => raise Fail ("todo: " ^ TU.toString (T.TupleTy ts))
    (* end case *))

  fun genMap (t as T.FunTy (alpha, beta)) = 
        if both FU.isInt (alpha, beta) then
          monoVarExp' DV.intMap
	else if both FU.isDouble (alpha, beta) then
          monoVarExp' DV.dblMap
	else if FU.isInt alpha then
          A.VarExp (DV.mapIFPoly (), [beta])
        else if FU.isGroundTy alpha then
          A.VarExp (DV.fmap (), [alpha, beta])
	else if FU.isInt_farray alpha andalso FU.isInt beta then
          monoVarExp' DV.map_IFF_IF
	else (case alpha
          of tup as T.TupleTy [t1, t2] =>
               if both FU.isGroundTy (t1, t2) then
	         if List.all FU.isInt [t1, t2, beta] then
                   monoVarExp' DV.ipMapEq_int
		 else let
                   val {seqMap, ropeMap, farrayMap} = SynthMap.synthFArrayMap (t1, t2, beta)
		   val A.FB (fmap, _, _) = farrayMap
                   fun fb lam = A.FunBind [lam]	
	           val binds = List.map fb [seqMap, ropeMap, farrayMap]
                   in
                     AU.mkLetExp (binds, monoVarExp fmap)
                   end
		 (* else A.VarExp (DV.fpmap (), [alpha, beta]) *)
	       else if FU.isInt_farray t1 andalso FU.isDbl_farray t2 andalso FU.isDouble beta then
                 (* (print "hit it\n"; raise Fail "halt") *)
	         monoVarExp' DV.map_IFF_DFF_DF
	       else raise Fail ("genMap(loc1) todo: " ^ TU.toString tup)
	   | _ => raise Fail ("genMap(loc2) todo: " ^ TU.toString t)
          (* end case *))
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

  fun genPairMap t = (case t
    of T.FunTy (fDomTy, fRngTy) => (case fDomTy
         of T.TupleTy [t1, t2] => 
              if List.all FU.isDouble [t1, t2, fRngTy] then
                A.VarExp (DV.fmapDDD (), [])
	      else
                raise Fail ("todo: " ^ TU.toString t)
	  | _ => raise Fail ("expected a pair: " ^ TU.toString t)
         (* end case *))
     | _ => raise Fail ("expected a function: " ^ TU.toString t)
    (* end case *))

  fun gen (pop : A.parray_op) : A.exp = let
(*  val _ = println ("*** generating " ^ PArrayOp.toString pop)  *)
    val g = (case pop
      of A.PA_Length ty => genLength ty
       | A.PA_Sub s => genSub s	
       | A.PA_Tab t => genTab t	     
       | A.PA_TabFTS t => genTabFTS t
       | A.PA_TabTupleFTS ts => genTabTupleFTS ts
       | A.PA_Map t => let
           val m = genMap t 
           in
	     print ("generated map for " ^ TU.toString t ^ "\n");
	     (* PrintAST.printExp m; *)
	     m
	   end
       | A.PA_Reduce t => genReduce t
       | A.PA_Range t => genRange t
       | A.PA_App t => genApp t
       | A.PA_TabHD (d, t) => genTabHD (d, t)
       | A.PA_PairMap t => genPairMap t
      (* end case *))
    in
(*    PrintAST.printExp g; *)
      g
    end

end

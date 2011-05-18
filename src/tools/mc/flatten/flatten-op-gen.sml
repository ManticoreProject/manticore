(* flatten-op-gen.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate functions, as AST, from fl_op values.
 *
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FlattenOpGen (* : sig

  val gen : FlattenOp.Set.set -> AST.lambda list

end *) = struct

  structure A = AST
  structure B = Basis
  structure T = Types
  structure F = FlattenOp

  structure AU = ASTUtil
  structure TU = TypeUtil
  structure FU = FlattenUtil

  structure D  = DelayedBasis
  structure DT = D.TyCon
  structure DD = D.DataCon
  structure DV = D.Var
  structure DTy = D.Ty

  structure FMap = F.Map
  structure FSet = F.Set

(* println : string -> unit *)
  fun println s = (print s; print "\n")

(* mapi : (('a * int) -> 'b) -> 'a list -> 'b list *)
(* Map a function that consumes the index of the current item too. *)
  fun mapi (f : ('a * int) -> 'b) (xs : 'a list) : 'b list  = let
    fun m (_, [], acc) = List.rev acc
      | m (i, x::xs, acc) = m (i+1, xs, f(x,i)::acc)
    in
      m (0, xs, [])
    end

(* some codegen helpers *)

  infix @@
  fun f @@ args = AU.mkApplyExp (f, args)

  infix -->
  fun d --> r = T.FunTy (d, r)

  infix <--
  fun p <-- e = A.ValBind (p, e)

  infix **
  fun f ** xs = List.map f xs

  fun vexp x = A.VarExp (x, [])

  fun mkPrintln s = (vexp (DV.println ())) @@ [AU.mkString s]

(* We record the lambdas we generate as we go in an (oper->lam) map, defined as an env. *)
(* We consult the env on the way in order to avoid generating code more than once per oper. *)
  type env = A.lambda FMap.map 

(* +debug *)
  fun ln () = TextIO.print "\n"
  fun println s = (TextIO.print s; ln ())

  val printFlops : env -> unit = (fn e => let
    val flops = map #1 (FMap.listItemsi e)
    val tos = FlattenOp.toString
    val s = String.concatWith "," (map tos flops)
    in
      println ("{" ^ s ^ "}")
    end)
(* -debug *)

(* unzip maker
 * Given a tuple type, this code generates a parallel unzip function as follows:
 * (for example) int * bool * char --> 
 * fun unzip(arg) = let
 *   val FArray(data,shape) = arg
 *   fun hash1(arg) = let val(x,_,_) = arg in x end
 *   fun hash2(arg) = let val(_,x,_) = arg in x end
 *   fun hash3(arg) = let val(_,_,x) = arg in x end
 *   in
 *     (| FArray(Rope.map(hash1,data), shape),
 *	  FArray(Rope.map(hash2,data), shape),
 *	  FArray(Rope.map(hash3,data), shape) |)
 *   end     
 *)
  local
  (* This seed is used for naming functions. *)
  (* Not necessary (since Var.new generates unique stamps anyway), but helpful. *)
    val seed = ref 0
    fun reset () = (seed := 0)
    fun incr () = (seed := (!seed)+1)
    fun freshName () = let
      val n = "unzip_" ^ Int.toString (!seed)
      in
	incr (); 
	n
      end
  (* mkHash : ty list * int -> lambda
   * Generate a selector function from the subtypes of a tuple type.
   * ex: mkHash ([int,bool,int], 0) --> 
   *   fun h0(arg) = let val (x:int,_:bool,_:int) = arg in x end
   * ex: mkHash ([int,bool,int], 1) --> 
   *   fun h1(arg) = let val (_:int,x:bool,_:int) = arg in x end *)
    fun mkHash (ts: T.ty list, i : int) : A.lambda = let
      fun lp (_, [], SOME x, ps) = (x, A.TuplePat (List.rev ps))
	| lp (j, t::ts, optX, ps) =
            if i=j then let
	      val x = Var.new ("x", t)
              in
                lp (j+1, ts, SOME x, A.VarPat(x)::ps)
              end        
	    else lp (j+1, ts, optX, A.WildPat(t)::ps)
	| lp _ = raise Fail "lp"
      val domTy = T.TupleTy ts
      val rngTy = List.nth (ts, i)
      val arg = Var.new ("arg", domTy)
      val f = Var.new ("hash_" ^ Int.toString i, T.FunTy (domTy, rngTy))
      val (x, p) = lp (0, ts, NONE, [])
      val body = A.LetExp (A.ValBind (p, A.VarExp (arg, [])),
			   A.VarExp (x, []))
(* +debug *)
val msg = mkPrintln ("running " ^ Var.nameOf f)
val body = A.SeqExp (msg, body) 
(* -debug *)
      in
        A.FB (f, arg, body)
      end

  (* mk : ty list -> lambda *)
  (* Make an unzip function for the given list of types. *)
    fun mk (ts : T.ty list) : A.lambda = let

    (* n.b. we've already checked that ts is of length 2 or greater *)

    (* choose a monomorphic farray representation if possible *)
      fun monomorphize (t : T.ty) : T.ty = (case t
        of T.ConTy ([t'], c) =>
             if FU.isInt t' andalso FU.isFArrayTyc c then
               DTy.int_farray ()
	     else if FU.isDouble t' andalso FU.isFArrayTyc c then
               DTy.dbl_farray ()
	     else t
	 | _ => t
        (* end case *))

      fun mkFArray (t : T.ty) : T.ty = 
        if FU.isInt t then
	  DTy.int_farray ()
	else if FU.isDouble t then
          DTy.dbl_farray ()
	else
	  DTy.farray t

    (* calculate the domain and range types for the function to be generated *)
      val domTy = T.ConTy ([T.TupleTy (List.map monomorphize ts)], DT.farray ())
      val rngTy = T.TupleTy (List.map mkFArray ts)

    (* create variables for function name and its argument *)
      val name  = freshName ()
      val unzip = Var.new (name, domTy --> rngTy)
(* +debug *)
val _ = println (concat ["building ", Var.nameOf unzip, ":", TU.toString (domTy --> rngTy)])
(* -debug *)
      val arg   = Var.new ("arg", domTy)

    (* build patterns against which to match the argument *)
    (* note: each must be a *simple* pattern, since match compilation is already done *)
      val dataTy   = DTy.rope (T.TupleTy ts)
      val shapeTy  = DTy.shape ()
      val data     = Var.new ("data", dataTy)
      val shape    = Var.new ("shape", shapeTy)
      val fArrPat  = A.ConPat (DD.farray (), 
			       [T.TupleTy ts], 
			       A.TuplePat [A.VarPat data, A.VarPat shape])

    (* generate typed selectors for each tuple component *)
      val hashes = List.tabulate (List.length ts, fn i => (mkHash (ts, i), i))

    (* mkMapHash : lambda -> exp *)
    (* function to consume a hash function and produce the expression
     *   FArray (Rope.map (hash, data), shape) 
     * where data and shape are the vars bound in the pattern match *)
      fun mkMapHash (hash as A.FB (h, _, _), i) = let 
        fun var v = A.VarExp (v, [])
        fun var1 (v, t) = A.VarExp (v (), [t])
	fun var2 (v, t1, t2) = A.VarExp (v (), [t1, t2])
        fun con0 c = A.DConst (c (), [])
	fun con1 (c, t) = A.DConst (c (), [t])
        val t = List.nth (ts, i)
	val mapExp = 
          if TU.same (t, B.intTy) then
            var1 (DV.ropeMap_int, T.TupleTy ts)
	  else if TU.same (t, B.doubleTy) then 
            var1 (DV.ropeMap_dbl, T.TupleTy ts)
	  else 
	    var2 (DV.ropeMap, T.TupleTy ts, t)
	val dcon = 
          if TU.same (t, B.intTy) then 
            con0 DD.intFArray
	  else if TU.same (t, B.doubleTy) then 
            con0 DD.dblFArray
	  else
            con1 (DD.farray, t)
        val m = AU.mkApplyExp (mapExp, [var h, var data])
        in
	  AU.mkApplyExp (A.ConstExp dcon, [m, var shape])
        end

      val binds = List.map (fn (h,_) => A.FunBind [h]) hashes
(*      val ptup = A.PTupleExp (List.map mkMapHash hashes) *)
      val ptup' = valOf (TranslatePtup.tr (fn e => e) (map mkMapHash hashes))
      val msg = mkPrintln ("running " ^ Var.nameOf unzip)
      val body = A.SeqExp (msg,
        AU.mkCaseExp (A.VarExp (arg, [T.TupleTy ts]),
		      [A.PatMatch (fArrPat, AU.mkLetExp (binds, ptup'))]))
      in
      (* all together now... *)
	AU.mkFunWithParams (unzip, [arg], body)
      end

  in
(* +debug
    fun hashtest () = let
      val hs = List.tabulate (3, fn i => mkHash ([B.intTy, B.boolTy, B.intTy], i))
      val bs = List.map (fn h => A.FunBind [h]) hs
      val e = AU.mkLetExp (bs, A.TupleExp [])
      in
	PrintAST.printExp e
      end
 * -debug *)
  (* mkUnzip : ty -> lambda *)
  (* pre: ty is an farray of tuples of length at least two *)
    fun mkUnzip (ty : T.ty) : A.lambda = (case ty
      of T.ConTy ([T.TupleTy (ts as _::_::_)], c) =>  
           if FU.isFArrayTyc c then 
             mk ts
	   else 
             raise Fail "mkUnzip: expecting farray"
       | _ => raise Fail ("mkUnzip: " ^ TU.toString ty)
      (* end case *))
  end

  local
  (* This seed is used for naming functions. *)
  (* Not necessary (since Var.new generates unique stamps anyway), but helpful. *)
    val seed = ref 0
    fun reset () = (seed := 0)
    fun incr () = (seed := (!seed)+1)
  (* pad with 0, 1 or 2 leading zeros *) 
    fun pad3 n = let
      val s = Int.toString n
      in case String.size s
        of 0 => raise Fail "pad3"
	 | 1 => "00" ^ s
	 | 2 => "0" ^ s
	 | _ => s
      end
    fun freshName () = let
      val idx = pad3 (!seed)
      val name = "fl_" ^ idx
      in
	incr(); name
      end
    fun nameOfLam (A.FB (f, _, _)) = Var.toString f
    fun typeOfLam (A.FB (f, _, _)) = Var.monoTypeOf f
  in
    fun resetOpNamer () = reset ()
    fun mkID ty = let
      val f = Var.new (freshName (), T.FunTy (ty, ty))
      val x = Var.new ("x", ty)
      val b = A.VarExp (x, []) 
      in
	A.FB (f, x, b)
      end
    fun mkMap (f : A.var) : A.lambda = let 
      val (fDomTy, fRngTy) = (case Var.monoTypeOf f
        of T.FunTy (d, r) => (d, r)
	 | t => raise Fail ("mkMap: " ^ TU.toString t))
      val domTy = DTy.farray fDomTy
      val rngTy = DTy.farray fRngTy
      val f = Var.new (freshName (), T.FunTy (domTy, rngTy))
      val arg = Var.new ("arg", domTy)
      val data = Var.new ("data", DTy.rope fDomTy)
      val shape = Var.new ("shape", DTy.shape ())
      val tpat = A.TuplePat [A.VarPat data, A.VarPat shape]
(* FIXME complex pattern binding -- change to case *)
      val pat = A.ConPat (DD.farray (), [domTy], tpat)
      val bind = A.ValBind (pat, A.VarExp (arg, []))
      val ropeMap = AU.mkApplyExp (A.VarExp (DV.ropeMap (), [fDomTy, fRngTy]),
				   [A.VarExp (f, []), A.VarExp (data, [])])
(* FIXME question: does the data VarExp need a type in its type list? *)
      val con = AU.mkApplyExp (A.ConstExp (A.DConst (DD.farray (), [fDomTy])),
			       [ropeMap, A.VarExp (shape, [])])
      val body = AU.mkLetExp ([bind], con)
      in 
        A.FB (f, arg, body)
      end
    fun composeLams (lam1 as A.FB (f, _, _), lam2 as A.FB (g, _, _)) = let
      fun v x   = A.VarExp (x, [])
      val domTy = TU.domainType (typeOfLam lam2)
      val rngTy = TU.rangeType (typeOfLam lam1)
      val name  = Var.nameOf f ^ "_o_" ^ Var.nameOf g
      val f_o_g = Var.new (name, domTy --> rngTy)
      val arg   = Var.new ("arg", domTy)
(* debugging and meta-debugging *)
val _ = println (concat ["building ", name, ":", TU.toString (domTy --> rngTy)])
val msg = mkPrintln ("running " ^ Var.nameOf f_o_g)
val body = A.SeqExp (msg, v f @@ [v g @@ [v arg]])
(* end debugging *)
      in
	AU.mkFunWithParams (f_o_g, [arg], body)
      end
    fun mkApps (x, ts, lams) = let
      fun v x = A.VarExp (x, [])
      val xs = mapi (fn (t,i) => Var.new ("x_" ^ Int.toString i, t)) ts
      fun mkApp (A.FB (f, _, _), x) = v f @@ [v x]
      val apps = AU.mkTupleExp (ListPair.mapEq mkApp (lams, xs))
      val tupPat = AU.mkTuplePat (A.VarPat ** xs)
      in
        AU.mkLetExp ([tupPat <-- v x], apps)
      end      
    fun mkCrossCompose lams = let
      val ts = typeOfLam ** lams
      val doms = TU.domainType ** ts
      val rngs = TU.rangeType ** ts
      val domTy = T.TupleTy doms
      val rngTy = T.TupleTy rngs
      val f = Var.new (freshName (), T.FunTy (domTy, rngTy))
      val x = Var.new ("x", domTy)
      val b = mkApps (x, doms, lams)
      in
        A.FB (f, x, b)
      end

(* FIXME I believe I may not be handling nested arrays of tuples correctly. *)
(* FIXME This is broken -- not working for int farr farr farr. *)
    fun mkCat domTy = 
          (
(* +debug *)
           print "mkCat called with domTy=";
	   print (TU.toString domTy);
	   print "\n";
(* -debug *) case domTy
      of T.ConTy ([T.ConTy ([t], _)], _) (* t farr farr *) => let
	   fun tvar x = A.VarExp (x, [t])
           val rngTy = DTy.farray t
	   val f = Var.new (freshName (), domTy --> rngTy)
	   val x = Var.new ("x", domTy)
	   val b = AU.mkApplyExp (tvar (DV.fflatten ()), [tvar x])
           in
	     A.FB (f, x, b)
	   end
       |  T.ConTy ([t], c) (* looking for int_farray farray *) =>
            if TU.same (t, DTy.int_farray ()) andalso FU.isFArrayTyc c then let
              (* TODO I'm committed to returning an FB, but that's not necessary in this case. *)
              (* I end up constructing an eta-expansion of the function in question. *)
	      (* How hard is this to fix? -ams *)
(* +debug *)
val _ = print ("found int_farray farray\n")
(* -debug *)  
              val rngTy = DTy.int_farray ()
	      fun ve x = A.VarExp (x, [])
	      val f = Var.new (freshName (), domTy --> rngTy)
	      val x = Var.new ("x", domTy)
	      val b = AU.mkApplyExp (ve (DV.flattenIFF ()), [ve x])
              in
                A.FB (f, x, b)
               end
	    else
	      raise Fail ("mkCat : " ^ TU.toString domTy)
       | _ => raise Fail ("mkCat : " ^ TU.toString domTy)
      (* end case *))

    fun mkOp env (oper : A.fl_op) : env * A.lambda =
     (case FMap.find (env, oper)
       of SOME lam => (env, lam)
	| NONE => (case oper
            of A.ID ty => let
                 val lam = mkID ty
		 val env' = FMap.insert (env, oper, lam)
                 in
		   (env', lam)
	         end           
	     | A.Unzip ty => let
(* +debug *)
		 (* val _ = println "&&&&&&&&&&&&&&&&" *)
		 (* val _ = println "making another unzip" *)
		 (* val _ = printFlops env *)
(* -debug *)
                 (* this type must be an array of tuple type *)
                 val lam = mkUnzip ty
		 val env' = FMap.insert (env, oper, lam)
                 in
		   (env', lam)
	         end
	     | A.Cat ty => let
		 (* this type should be a nested farr of depth 2 *)
		 (* - that is, as in [[int]], not [int] or [[[int]]] *)
		 (* this condition should have been respected in FlattenOp.construct *)
                 val lam = mkCat ty
                 val env' = FMap.insert (env, oper, lam)
	         in
		   (env', lam)
	         end
	     | A.Map (o1, n) => let
                 val (env', A.FB (f, _, _)) = mkOp env o1
		 val lam = mkMap f
                 val env'' = FMap.insert (env', o1, lam)
                 in
		   (env'', lam)
	         end	
	     | A.Compose (o1, o2) => let
		 val (env', lam1) = mkOp env o1
		 val (env'', lam2) = mkOp env' o2
		 val lam = composeLams (lam1, lam2)
		 val env''' = FMap.insert (env'', oper, lam)
                 in
		   (env''', lam)
	         end
	     | A.CrossCompose os => let
                 fun lp ([], env', lams) = (env', List.rev lams)
		   | lp (h::t, env, lams) = let
                       val (env', lam) = mkOp env h
                       in
                         lp (t, env', lam::lams)
                       end
                 val (env', lams) = lp (os, env, [])
		 val lam = mkCrossCompose lams
		 val env'' = FMap.insert (env', oper, lam)
                 in
		   (env'', lam)
	         end
            (* end case *))
       (* end case *))
  end (* local *)
	     
  fun gen (s : FSet.set) : (A.fl_op * A.lambda) list = let
    val env0 : env = FMap.empty
    fun lp ([], env) = FMap.listItemsi env
      | lp (h::t, env) = let
          val (env', lam) = mkOp env h
          in
	    lp (t, env')
	  end
    in
      resetOpNamer () (* reset int seed for function naming (not strictly necessary) *);
      lp (FSet.listItems s, env0)
    end

end

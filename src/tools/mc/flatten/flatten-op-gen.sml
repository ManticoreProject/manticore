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

  structure FMap = F.Map
  structure FSet = F.Set

(* mapi : (('a * int) -> 'b) -> 'a list -> 'b list *)
(* Map a function that consumes the index of the current item too. *)
  fun mapi (f : ('a * int) -> 'b) (xs : 'a list) : 'b list  = let
    fun m (_, [], acc) = List.rev acc
      | m (i, x::xs, acc) = m (i+1, xs, f(x,i)::acc)
    in
      m (0, xs, [])
    end

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

(* basisItems : unit -> {fArrTyc, fArrCon, ntreeTyc, ropeTyc, ropeMap} *)
(* Trying to bind these at the top level causes a link-time failure,   *)
(*   so we provide a memoized, thunkified collection of them.          *)
  local
    val cell = ref NONE
  in
    fun basisItems () = (case !cell
      of SOME record => record 
       | NONE => let
	   val ft = BasisEnv.getTyConFromBasis ["FArray", "f_array"]
	   val fc = BasisEnv.getDConFromBasis  ["FArray", "FArray"]
	   val nt = BasisEnv.getTyConFromBasis ["FArray", "nesting_tree"]
	   val ff = BasisEnv.getVarFromBasis ["FArray", "flatten"]
	   val rt = BasisEnv.getTyConFromBasis ["Rope", "rope"]
	   val rm = BasisEnv.getVarFromBasis   ["Rope", "mapP"]
           val record = {fArrTyc=ft, fArrCon=fc, fArrFlatten=ff,
			 ntreeTyc=nt, ropeTyc=rt, ropeMap=rm}
           in
             cell := SOME record;
	     record
           end
       (* end case *))
  end (* local *)

(* +debug *)
  local
    val ar = TyVar.new (Atom.atom "'a")
    val rt = TyCon.newDataTyc (Atom.atom "rope", [ar])
    val nt = TyCon.newDataTyc (Atom.atom "nesting_tree", [])
    val a  = TyVar.new (Atom.atom "'a")
    val ft = TyCon.newDataTyc (Atom.atom "f_array", [a])
    val fc = DataCon.new ft (Atom.atom "FArray", 
			     SOME (T.TupleTy [T.ConTy ([T.VarTy a], rt),
					      T.ConTy ([], nt)]))
    val rm = let
      val br = TyVar.new (Atom.atom "'b")
      val dom = T.TupleTy [T.FunTy (T.VarTy ar, T.VarTy br),
			   T.ConTy ([T.VarTy ar], rt)]
      val rng = T.ConTy ([T.VarTy br], rt)
      val sch = T.TyScheme ([ar, br], T.FunTy (dom, rng))
      in 
        Var.newPoly ("Rope_map", sch)
      end
    val ff = let
      val a = TyVar.new (Atom.atom "'a")
      val dom = T.ConTy ([T.ConTy ([T.VarTy a], ft)], ft)
      val rng = T.ConTy ([T.VarTy a], ft)
      val sch = T.TyScheme ([a], T.FunTy (dom, rng))
      in
	Var.newPoly ("FArray_flatten", sch)
      end
  in
    fun spoofBasisItems () = 
      {fArrTyc=ft, fArrCon=fc, fArrFlatten=ff,
       ntreeTyc=nt, ropeTyc=rt, ropeMap=rm}
  end (* local *)      
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
      in
        A.FB (f, arg, body)
      end

  (* mk : ty list -> lambda *)
  (* Make an unzip function for the given list of types. *)
    fun mk (ts : T.ty list) : A.lambda = let

    (* n.b. we've already checked that ts is of length 2 or greater *)
    (* we'll need some tycons, dcons and vars from the basis *)
    (* n.b. trying to bind these at the top level causes a link-time failure *)
      val {fArrTyc, fArrCon, fArrFlatten, ntreeTyc, ropeTyc, ropeMap} = basisItems ()

    (* calculate the domain and range types for the function to be generated *)
      val domTy = T.ConTy ([T.TupleTy ts], fArrTyc)
      val rngTy = T.TupleTy (List.map (fn t => T.ConTy ([t], fArrTyc)) ts)

    (* create variables for function name and its argument *)
      val name  = freshName ()
      val unzip = Var.new (name, T.FunTy (domTy, rngTy))
      val arg   = Var.new ("arg", domTy)

    (* build patterns against which to match the argument *)
    (* note: each must be a *simple* pattern, since match compilation is already done *)
      val dataTy   = T.ConTy ([T.TupleTy ts], ropeTyc)
      val shapeTy  = T.ConTy ([], ntreeTyc)
      val data     = Var.new ("data", dataTy)
      val shape    = Var.new ("shape", shapeTy)
      val fArrPat = A.ConPat (fArrCon, 
			      [T.TupleTy ts], 
			      A.TuplePat [A.VarPat data, A.VarPat shape])

    (* generate typed selectors for each tuple component *)
      val hashes = List.tabulate (List.length ts, fn i => (mkHash (ts, i), i))

    (* mkMapHash : lambda -> exp *)
    (* function to consume a hash function and produce the expression
     *   FArray (Rope.map (hash, data), shape) 
     * where data and shape are the vars bound in the pattern match *)
      fun mkMapHash (hash as A.FB (h, _, _), i) = let 
        val t = List.nth (ts, i)
        val m = AU.mkApplyExp (A.VarExp (ropeMap, 
					 [T.TupleTy ts, t]),
			      [A.VarExp (h, []), 
			       A.VarExp (data, [])])
        in
	  AU.mkApplyExp (A.ConstExp (A.DConst (fArrCon, [t])), 
			 [m, A.VarExp (shape, [])])
        end
      val binds = List.map (fn (h,_) => A.FunBind [h]) hashes
      val ptup = A.PTupleExp (List.map mkMapHash hashes)
      val body = A.CaseExp (A.VarExp (arg, [T.TupleTy ts]),
			    [A.PatMatch (fArrPat, AU.mkLetExp (binds, ptup))],
			    rngTy)
      in
      (* all together now... *)
        A.FB (unzip, arg, body)
      end

      fun isFArrayTyc c = let
        val {fArrTyc, ...} = basisItems ()
        in
	  TyCon.same (fArrTyc, c)
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
           if isFArrayTyc c then mk ts
	   else raise Fail "mkUnzip: expecting farray"
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
      val {fArrTyc, fArrCon, fArrFlatten, ntreeTyc, ropeTyc, ropeMap} = basisItems ()
      val (fDomTy, fRngTy) = (case Var.monoTypeOf f
        of T.FunTy (d, r) => (d, r)
	 | t => raise Fail ("mkMap: " ^ TU.toString t))
      val domTy = T.ConTy ([fDomTy], fArrTyc)
      val rngTy = T.ConTy ([fRngTy], fArrTyc)
      val f = Var.new (freshName (), T.FunTy (domTy, rngTy))
      val arg = Var.new ("arg", domTy)
      val data = Var.new ("data", T.ConTy ([fDomTy], ropeTyc))
      val shape = Var.new ("shape", T.ConTy ([], ntreeTyc))
      val tpat = A.TuplePat [A.VarPat data, A.VarPat shape]
(* FIXME complex pattern binding -- change to case *)
      val pat = A.ConPat (fArrCon, [domTy], tpat)
      val bind = A.ValBind (pat, A.VarExp (arg, []))
      val ropeMap = AU.mkApplyExp (A.VarExp (ropeMap, [fDomTy, fRngTy]),
				   [A.VarExp (f, []), A.VarExp (data, [])])
(* FIXME question: does the data VarExp need a type in its type list? *)
      val con = AU.mkApplyExp (A.ConstExp (A.DConst (fArrCon, [fDomTy])),
			       [ropeMap, A.VarExp (shape, [])])
      val body = AU.mkLetExp ([bind], con)
      in 
        A.FB (f, arg, body)
      end
    fun mkCompose (lam1, lam2) = let
      val A.FB (g, _, _) = lam1
      val A.FB (h, _, _) = lam2
      val domTy = TU.domainType (typeOfLam lam2)
      val rngTy = TU.rangeType (typeOfLam lam1)
      val f = Var.new (freshName (), T.FunTy (domTy, rngTy))
      val x = Var.new ("x", domTy)
      val b = ASTUtil.mkApplyExp (A.VarExp (g, []),
				  [ASTUtil.mkApplyExp (A.VarExp (h, []),
						       [A.VarExp (x, [])])])
      in
	A.FB (f, x, b)
      end
    fun mkApps (x, ts, lams) = let
      val xs = mapi (fn (t,i) => Var.new ("x_" ^ Int.toString i, t)) ts
      fun mkApp (lam, x) = let
        val A.FB (f, _, _) = lam
        in
	  ASTUtil.mkApplyExp (A.VarExp (f, []), 
			      [A.VarExp (x, [])])
        end
      val apps = A.TupleExp (ListPair.mapEq mkApp (lams, xs))
      val tupPat = A.TuplePat (List.map A.VarPat xs)
      in
        A.LetExp (A.ValBind (tupPat, A.VarExp (x, [])), apps)
      end      
    fun mkCrossCompose lams = let
      val ts = List.map typeOfLam lams
      val doms = List.map TU.domainType ts
      val domTy = T.TupleTy doms
      val rngTy = T.TupleTy (List.map TU.rangeType ts)
      val f = Var.new (freshName (), T.FunTy (domTy, rngTy))
      val x = Var.new ("x", domTy)
      val b = mkApps (x, doms, lams)
      in
        A.FB (f, x, b)
      end

    fun mkCat domTy = (case domTy
(* FIXME I believe I may not be handling nested arrays of tuples correctly. *)
(* FIXME This is broken. Try int farr farr farr. FIX!!! *)
      of T.ConTy ([T.ConTy ([t], _)], _) (* t farr farr *) => let
           val {fArrTyc, fArrFlatten, ...} = basisItems ()
	   val rngTy = T.ConTy ([t], fArrTyc)
	   val f = Var.new (freshName (), T.FunTy (domTy, rngTy))
	   val x = Var.new ("x", domTy)
	   val b = AU.mkApplyExp (A.VarExp (fArrFlatten, [t]), [A.VarExp (x, [t])])
           in
	     A.FB (f, x, b)
	   end
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
		 val lam = mkCompose (lam1, lam2)
		 val env''' = FMap.insert (env'', oper, lam)
                 in
		   (env''', lam)
	         end
	     | A.CrossCompose os => let
                 fun lp ([], env', lams) = (env', lams)
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

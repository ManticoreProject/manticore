(* translate-parr.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslateParr (* : sig

  (* An AST to BOM translation of parrays to ropes. *)
  (* tr : (TranslateEnv.env * (A.STexp -> BOM.exp)) -> (AST.exp list * AST.ty) -> BOM.exp *)
    val tr : (TranslateEnv.env * (AST.exp -> BOM.exp)) -> (AST.exp list * AST.ty) -> BOM.exp 

  end *) = struct

    structure A = AST
    structure B = Basis
    structure R = Ropes

    val maxLeafSize = Ropes.maxLeafSize

    datatype 'a leaf 
      = Lf of (int * 'a list)
              (* length, data *)

    datatype 'a rope 
      = Leaf of 'a leaf
      | Cat  of (int * int * 'a rope * 'a rope)
		(* length, depth, left, right *)

  (* Note the length and depth of ropes are precomputed, so length and depth *)
  (* are constant-time calculations. *)

    val emptyRope = Leaf (Lf (0, []))

  (* ropeLen : 'a rope -> nat *)
  (* Returns the number of elements of data at the leaves of the given rope. *)
    fun ropeLen (Leaf (Lf (n, _))) = n
      | ropeLen (Cat (n, _, _, _)) = n

  (* ropeDepth : 'a rope -> nat *)
  (* Returns the depth (length of the longest path to a leaf) of given rope. *)
  (* Note that by definition a leaf has depth 0. *)
    fun ropeDepth (Leaf _) = 0
      | ropeDepth (Cat (_, d, _, _)) = d

  (* isEmpty : 'a rope -> bool *)
    fun isEmpty r = (ropeLen r = 0)

  (* takeDrop : 'a list * nat -> 'a list * 'a list *)
  (* A function to do a "take" and a "drop" in one pass. *)
  (* ex: takeDrop ([5,6,7,8,9],2) --> ([5,6],[7,8,9]) *)
  (* Does not raise an exception if it runs out of things to take. *)
    fun takeDrop (xs, n) =
	let fun build (_, ekat, []) = (rev ekat, [])
	      | build (n, ekat, drop as d::ds) = 
		  if n = 0
		  then (rev ekat, drop)
		  else build (n-1, d::ekat, ds)
	in
	    build (n, [], xs)
	end
  
  (* mergeLeaves : 'a leaf * 'a leaf -> 'a rope *)
  (* This function merges two leaves into a single leaf if the total *)
  (* number of data elements does not exceed maxLeafSize. *)
  (* Otherwise, it packs maxLeafSize-worth of data into a left leaf, *)
  (* the rest into a right leaf, and concatenates them. *)
    fun mergeLeaves (Lf (len1, xs1), Lf (len2, xs2)) =
	let val totalLen = len1 + len2
	    val xs = xs1 @ xs2
	in
	    if totalLen <= maxLeafSize
	    then Leaf (Lf (totalLen, xs))
	    else let val (leftData, rightData) = takeDrop (xs, maxLeafSize)
		     val left  = Lf (maxLeafSize, leftData) 
		     val right = Lf (totalLen - maxLeafSize, rightData)
		 in
		     Cat (totalLen, 1, Leaf left, Leaf right)
		 end
	end
  
  (* cat : 'a rope * 'a rope -> 'a rope *)
  (* This constructor is not smart at all, i.e., does *no* analysis of its args. *)
  (* It is appropriate for use when a tree is known in advance to be balanced. *)
    fun cat (r1, r2) = 
	let val len = ropeLen r1 + ropeLen r2
	    val d = 1 + (Int.max (ropeDepth r1, ropeDepth r2))
	in
	    Cat (len, d, r1, r2)
	end

  (* mkCat : 'a rope * 'a rope -> 'a rope *)
  (* This is a slightly smart constructor for ropes. *)
    fun mkCat (r1, r2) =
	if isEmpty r1 then r2
	else if isEmpty r2 then r1
	else (case (r1, r2)
		of (Leaf L1, Leaf L2) => mergeLeaves (L1, L2)
		 | _ => cat (r1, r2)
	       (* end case *))

  (* smartCat : 'a rope * 'a rope -> 'a rope *)
  (* A smart constructor (i.e., with balancing) for ropes. *)
    fun smartCat (r1, r2) = raise Fail "todo: smartCat"
  
  (* ropeFromExps : A.exp list -> A.exp rope *)
  (* Consumes a list of expressions and produces an ideally-balanced *)
  (* rope from them. *)
    fun ropeFromExps es =
	  let (* makeLeaf : 'a list -> 'a rope *)
	      (* pre: length xs does not exceed maxLeafSize *)
	      fun makeLeaf xs = Leaf (Lf (length xs, xs))
	      (* makeLeaves : 'a list -> 'a rope list *)
	      (* Chop up a list and put the data into leaves. *)
	      fun makeLeaves xs =
		  let fun b ([], acc) = rev acc
			| b (xs, acc) =
		          let val (xsFront, xsBack) = takeDrop (xs, maxLeafSize)
			      val leaf = makeLeaf xsFront
			  in
			      case xsBack
			       of [] => rev (leaf :: acc)
				| _ => b (xsBack, leaf :: acc)
			  end
		  in
		      b (xs, [])
		  end
	      (* catPairs : 'a rope list -> 'a rope list *)
	      (* Concatenate the ropes in a list, two at a time. *)
	      fun catPairs [] = []
		| catPairs [r] = [r]
		| catPairs (r1::r2::rs) = mkCat (r1, r2) :: catPairs rs  
	      (* ropeFromRopeList : 'a rope list -> 'a rope *)
	      (* Iterates pairwise concatenation of ropes till there *)
	      (* is one rope only. *)
	      fun ropeFromRopeList []  = emptyRope
		| ropeFromRopeList [r] = r
		| ropeFromRopeList rs  = ropeFromRopeList (catPairs rs)
	  in
	      ropeFromRopeList (makeLeaves es)
	  end

    local
	val mkInt  = ASTUtil.mkInt
	val mkList = ASTUtil.mkList
	val catTupTy = 
	    let val intTy  = BOMBasis.intTy
		val ropeTy = BOMBasis.ropeTy
	    in
		BOMTy.T_Tuple (false, [intTy, intTy, ropeTy, ropeTy])
	    end
    in
    fun ropeBOM (env, trExpToV) (r, t) =
	let val handler = TranslateEnv.handlerOf env
	in case r
	     of Leaf (Lf (len, data)) =>
	          let val leafAST = R.ropeLeafExp t
		      val lenAST  = mkInt len
		      val dataAST = mkList (data, t)
		      val tupAST = A.TupleExp [lenAST, dataAST]
		  in
                      trExpToV (env, leafAST, fn LEAF =>
		        trExpToV (env, tupAST, fn tup =>
                          BOM.mkApply (LEAF, [tup], [handler])))
		  end
	      | Cat (n, d, r1, r2) =>
	          let val catAST = R.ropeCatExp t
                      val nAST = mkInt n
		      val dAST = mkInt d
		      val r1BOM = ropeBOM (env, trExpToV) (r1, t)
		      val r2BOM = ropeBOM (env, trExpToV) (r2, t)
		      val r1var = BOM.Var.new ("r1", BOMBasis.ropeTy)
		      val r2var = BOM.Var.new ("r2", BOMBasis.ropeTy)
		      val tupvar = BOM.Var.new ("_tup", catTupTy)			  
		  in
                      trExpToV (env, catAST, fn CAT =>
		        trExpToV (env, nAST, fn n =>
                          trExpToV (env, dAST, fn d =>
                            BOM.mkLet ([r1var], r1BOM,
                              BOM.mkLet ([r2var], r2BOM,
                                BOM.mkLet ([tupvar], let val t = BOM.Var.new ("t", catTupTy)
							 val vs = [n, d, r1var, r2var]
						     in
							 BOM.mkStmt([t], 
								    BOM.E_Alloc(catTupTy, vs),
								    BOM.mkRet [t])
						     end,
      			          BOM.mkApply (CAT, [tupvar], [handler])))))))
		  end
	end
    end (* local *)

  (* tr : (TranslateEnv.env * (A.exp -> BOM.exp)) -> exp rope -> BOM.exp *)
    fun tr (env, trExpToV) (es, t) = ropeBOM (env, trExpToV) (ropeFromExps es, t)

  end

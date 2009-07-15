(* rewrites.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Defines basics, including the rewrite grammar, of the HLOp rewrite
 * infrastructure.
 *)

structure Rewrites = struct

    structure BTy = BOMTy

    datatype rw_elt =
	     Elt_HLOp of BOM.hlop
	   | Elt_Prim of BOM.var
	   | Elt_Alloc
	   | Elt_Wildcard

    fun eltToString (Elt_HLOp hlop) = HLOp.toString hlop
      | eltToString (Elt_Prim p) = BOM.Var.toString p
      | eltToString Elt_Alloc = "alloc"
      | eltToString Elt_Wildcard = "*"

    fun compareElt (Elt_HLOp h1, Elt_HLOp h2) = HLOp.compare (h1, h2)
      | compareElt (Elt_Prim p1, Elt_Prim p2) = BOM.Var.compare (p1, p2)
      | compareElt (Elt_Prim _, Elt_HLOp _) = LESS
      | compareElt (Elt_HLOp _, Elt_Prim _) = GREATER
      | compareElt (Elt_Alloc, Elt_Alloc) = EQUAL
      | compareElt (Elt_Alloc, _) = LESS
      | compareElt (_, Elt_Alloc) = GREATER
      | compareElt (Elt_Wildcard, Elt_Wildcard) = EQUAL
      | compareElt (Elt_Wildcard, _) = LESS
      | compareElt (_, Elt_Wildcard) = GREATER

    fun sameElt (e1, e2) = compareElt (e1, e2) = EQUAL

    structure EltKey =
      struct
	type ord_key = rw_elt
	val compare = compareElt
      end
    structure EltMap = RedBlackMapFn (EltKey)

    datatype rw_pattern = datatype BOM.rw_pattern

    datatype rewrite = datatype BOM.rewrite

    datatype production = HLRWProduction of {
        name : rw_elt,
        rhs : rw_elt list,
        rw_opt : rewrite option
      }

    datatype grammar = HLRWGrammar of production list

    (* newGrammar() - Abstraction for creating a rewrite grammar with no
       productions. *)
    fun newGrammar () = HLRWGrammar []

    (* addProductionToGrammar() - Abstraction function for adding a
       HLRW production to a grammar.

       Currently just cons' the passed prod to the grammar's list. *)
    fun addProductionToGrammar (prod, grammar as HLRWGrammar prod_list) =
        HLRWGrammar (prod :: prod_list)

    (* getNewNonterminal() - Another abstraction for getting a new
       nonterminal name w.r.t. the given grammar. *)
    fun getNewNonterminal (grammar as HLRWGrammar prod_list) =
        Elt_Prim (BOM.Var.new ("!NT" ^ (Int.toString (length prod_list)), BOMTy.T_Any))

    (* matchRHS() - Match a production against a list of nonterminals. *)
    fun matchRHS (HLRWProduction {rhs, ...}, rhs') = let
        val similarSize = (List.length rhs) = (List.length rhs')
    in
        if similarSize
        then ListPair.foldl (fn (a1, a2, acc) =>
                                acc andalso (EltKey.compare(a1, a2) = EQUAL))
                            true (rhs, rhs')
        else similarSize
    end (* matchRHS() *)

    (* similarProduction() - Return true if the given production
       matches the other RHS, and both optional rewrites are none. *)
    fun similarProduction (prod as HLRWProduction {name, rhs, rw_opt}, rhs',
                           rw_opt') = let
        (* This is simplified so I don't have to define an order on
           rewrites. *)
        val similarRW = not ((isSome rw_opt) orelse (isSome rw_opt'))
    in
        if similarRW then matchRHS(prod, rhs') else similarRW
    end (* similarProduction() *)

    (* addPatternToGrammar() - Add a rewrite pattern as a production
       in the grammar, returning the non-terminal and new grammar. *)
    fun addPatternToGrammar (pat, grammar, rw_opt) : rw_elt * grammar = let

        fun rwFolder (p, (alist, g)) = let
            val (prod_name, g') = addPatternToGrammar(p, g, NONE)
        in
            (prod_name :: alist, g')
        end (* rwFolder() *)

        (* matchProduction() - See if an identical production doesn't already
           exist.  *)
        fun matchProduction (rhs : rw_elt list, HLRWGrammar prods) = let
            fun matchProduction' (rhs,
                                  (prod as HLRWProduction { name, ... }) ::
                                  prods) =
                if similarProduction(prod, rhs, rw_opt)
                then SOME name
                else matchProduction' (rhs, prods)
              | matchProduction' (rhs, []) = NONE
        in
            matchProduction'(rhs, prods)
        end (* matchProduction() *)

        (* addProduction() - Create a new production, unless a dupe
           already exists, and add to the given grammar. *)
        fun addProduction (rhs, grammar) =
            (case matchProduction (rhs, grammar)
              of SOME prod_name => (prod_name, grammar)
               | NONE => let
                     val prod_name = getNewNonterminal grammar
                     val prod = HLRWProduction { name = prod_name,
                                                 rhs = rhs,
                                                 rw_opt = rw_opt }
                 in
                     (prod_name, addProductionToGrammar (prod, grammar))
                 end
             (* end case *))

    in case pat
        of RW_HLOpApply (fname, pats) => let
               val (argNTs, grammar') = foldl rwFolder ([], grammar) pats
           in
               addProduction(Elt_HLOp fname :: (rev argNTs), grammar')
           end
         | RW_Const(_,_) =>
           raise (Fail
               "Constants not currently supported in LHS of rewrite patterns.")
         | RW_Var(_) => (Elt_Wildcard, grammar)
	 | RW_Alloc pats => let
               val (argNTs, grammar') = foldl rwFolder ([], grammar) pats
           in
	       (Elt_Alloc, grammar')
	   end
    end (* addPatternToGrammar() *)

    (* addRWToGrammar() - Convert a rewrite definition to a set of
       productions, and use these to extend the given grammar. *)
    fun addRWToGrammar (rw as Rewrite {lhs,...}, grammar : grammar) =
        #2 (addPatternToGrammar(lhs, grammar, SOME rw))

    (* rwOptToString() - Convert an optional rewrite to a label, weight pair.

       XXX Should this be moved to HLRWDefPT? *)
    fun rwOptToString (SOME (Rewrite {label, weight, ...})) =
        String.concat ["{", IntInf.toString weight, ", ", Atom.toString label,
                       "}"]
      | rwOptToString NONE = "{0, -}"

    (* getGrammarHash() - Given a grammar, create a mapping from the
       first atom in each production's right hand side to a list of
       productions that have that atom as the first RHS token. *)
    fun getGrammarHash (HLRWGrammar prods) = let
        fun prodHashFolder (prod as HLRWProduction {rhs, ...}, m) = let
            val prodKey = hd rhs
            val prods' = (case EltMap.find(m, prodKey)
                           of SOME prods => prod::prods
                            | NONE => [prod]
                           (* end case *))
        in
            EltMap.insert(m, prodKey, prods')
        end (* prodHashFolder() *)
    in
        foldl prodHashFolder EltMap.empty prods
    end (* getGrammarHash() *)

    (* getGrammarProductionMap() - Create a map from nonterminals in
       the grammar to their (supposedly sole) production. *)
    fun getGrammarProductionMap (HLRWGrammar prods) = let
        fun prodMapFolder (prod as HLRWProduction {name, ...}, m) =
            EltMap.insert(m, name, prod)
    in
        foldl prodMapFolder EltMap.empty prods
    end (* getGrammarProductionMap() *)

    (* getProductionWeight() - Calculate the weight of the given
       production (not counting benefit of children), which will either
       be zero, or based on the rewrite weight. *)
    fun getProductionWeight (HLRWProduction {rw_opt, ...}) = (case rw_opt
        of NONE => IntInf.fromInt 0
         | SOME (Rewrite {weight, ...}) => weight
        (* end case *))

    (* productionHasRW() - Return true if the given production has a
       rewrite associated with it, false otherwise. *)
    fun productionHasRW (HLRWProduction {rw_opt = SOME rw, ... }) = true
      | productionHasRW _ = false

    (* productionToString() - Create a string representation of a RW grammar
       production. *)
    fun productionToString (HLRWProduction {name, rhs, rw_opt}) = let
        val rhss = String.concat (List.map (fn a => (eltToString a) ^ " ")
                                           rhs)
    in
        String.concat [eltToString name, " := ", rhss, rwOptToString rw_opt]
    end (* productionToString *)

    (* grammarToString() - Create a string representation of a full RW grammar
       (as newline delimited productions). *)
    fun grammarToString (HLRWGrammar nil) = "Empty rewrite grammar.\n"
      | grammarToString (HLRWGrammar rewrites) = let
        val prodStrs = List.map (fn ps => (productionToString ps) ^ "\n")
                                rewrites
    in
        String.concat prodStrs
    end (* grammarToString *)

    (* patternToString() - Utility function to make a string from a
       parse tree pattern. *)
    fun patternToString (RW_HLOpApply(name, pats)) =
        String.concat [ HLOp.toString name, "(",
                        String.concatWith ", " (List.map patternToString pats),
                        ")" ]
        (* FIXME: something to print the constant type? *)
      | patternToString (RW_Const(lit, ty)) = Literal.toString lit
      | patternToString (RW_Var(name)) = BOM.Var.toString name
      | patternToString (RW_Alloc pats) = 
	"alloc (" ^ String.concatWith ", " (List.map patternToString pats) ^ ")"

    (* rewriteToString() - Utility function to make a string from a
       parse tree rewrite. *)
    fun rewriteToString (Rewrite {label, lhs, rhs, weight}) =
        String.concat [ Atom.toString label, " : ", patternToString lhs,
                        " ==> ", patternToString rhs, " {",
                        IntInf.toString weight, "}" ]

end (* Rewrites *)

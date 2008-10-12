(* rewrites.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Defines basics, including the rewrite grammar, of the HLOp rewrite
 * infrastructure.
 *)

structure Rewrites = struct

    structure PT = HLRWDefPT
    structure BTy = BOMTy

    val wildcard = Atom.atom "*"

    datatype production = HLRWProduction of {
        name : Atom.atom,
        rhs : Atom.atom list,
        rw_opt : HLRWDefPT.rewrite option
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
        Atom.atom ("!NT" ^ (Int.toString (length prod_list)))

    (* matchRHS() - Match a production against a list of nonterminals. *)
    fun matchRHS (HLRWProduction {rhs, ...}, rhs') = let
        val similarSize = (List.length rhs) = (List.length rhs')
    in
        if similarSize
        then ListPair.foldl (fn (a1, a2, acc) =>
                                acc andalso (Atom.same(a1, a2)))
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
    fun addPatternToGrammar (pat, grammar, rw_opt) = let

        fun rwFolder (p, (alist, g)) = let
            val (prod_name, g') = addPatternToGrammar(p, g, NONE)
        in
            (prod_name :: alist, g')
        end (* rwFolder() *)

        (* matchProduction() - See if an identical production doesn't already
           exist.  *)
        fun matchProduction (rhs : Atom.atom list, HLRWGrammar prods) = let
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
        of HLRWDefPT.Call(fname, pats) => let
               val (argNTs, grammar') = foldl rwFolder ([], grammar) pats
           in
               addProduction(fname :: (rev argNTs), grammar')
           end
         | HLRWDefPT.Const(_,_) =>
           raise (Fail
               "Constants not currently supported in LHS of rewrite patterns.")
         | HLRWDefPT.Var(_) => (wildcard, grammar)
    end (* addPatternToGrammar() *)

    (* addRWToGrammar() - Convert a rewrite definition to a set of
       productions, and use these to extend the given grammar. *)
    fun addRWToGrammar (rw as PT.Rewrite {lhs,...}, grammar : grammar) =
        #2 (addPatternToGrammar(lhs, grammar, SOME rw))

    (* rwOptToString() - Convert an optional rewrite to a label, weight pair.

       XXX Should this be moved to HLRWDefPT? *)
    fun rwOptToString (SOME (HLRWDefPT.Rewrite {label, weight, ...})) =
        String.concat ["{", IntInf.toString weight, ", ", Atom.toString label,
                       "}"]
      | rwOptToString NONE = "{0, -}"

    (* getGrammarHash() - Given a grammar, create a mapping from the
       first atom in each production's right hand side to a list of
       productions that have that atom as the first RHS token. *)
    fun getGrammarHash (HLRWGrammar prods) = let
        fun prodHashFolder (prod as HLRWProduction {rhs, ...}, m) = let
            val prodKey = hd rhs
            val prods' = (case AtomMap.find(m, prodKey)
                           of SOME prods => prod::prods
                            | NONE => [prod]
                           (* end case *))
        in
            AtomMap.insert(m, prodKey, prods')
        end (* prodHashFolder() *)
    in
        foldl prodHashFolder AtomMap.empty prods
    end (* getGrammarHash() *)

    (* getGrammarProductionMap() - Create a map from nonterminals in
       the grammar to their (supposedly sole) production. *)
    fun getGrammarProductionMap (HLRWGrammar prods) = let
        fun prodMapFolder (prod as HLRWProduction {name, ...}, m) =
            AtomMap.insert(m, name, prod)
    in
        foldl prodMapFolder AtomMap.empty prods
    end (* getGrammarProductionMap() *)

    (* getProductionWeight() - Calculate the weight of the given
       production (not counting benefit of children), which will either
       be zero, or based on the rewrite weight. *)
    fun getProductionWeight (HLRWProduction {rw_opt, ...}) = (case rw_opt
        of NONE => IntInf.fromInt 0
         | SOME (HLRWDefPT.Rewrite {weight, ...}) => weight
        (* end case *))

    (* productionHasRW() - Return true if the given production has a
       rewrite associated with it, false otherwise. *)
    fun productionHasRW (HLRWProduction {rw_opt = SOME rw, ... }) = true
      | productionHasRW _ = false

    (* findProduction() - Find the (supposedly sole) production for the given
       nonterminal in the grammar. *)
    fun findProduction (nt, HLRWGrammar prods) =
        List.find (fn HLRWProduction {name, ...} => Atom.same(nt, name)) prods

    (* productionToString() - Create a string representation of a RW grammar
       production. *)
    fun productionToString (HLRWProduction {name, rhs, rw_opt}) = let
        val rhss = String.concat (List.map (fn a => (Atom.toString a) ^ " ")
                                           rhs)
    in
        String.concat [Atom.toString name, " := ", rhss, rwOptToString rw_opt]
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
    fun patternToString (PT.Call(name, pats)) =
        String.concat [ Atom.toString name, "(",
                        String.concatWith ", " (List.map patternToString pats),
                        ")" ]
        (* FIXME: something to print the constant type? *)
      | patternToString (PT.Const(lit, ty)) = Literal.toString lit
      | patternToString (PT.Var(name)) = Atom.toString name

    (* rewriteToString() - Utility function to make a string from a
       parse tree rewrite. *)
    fun rewriteToString (PT.Rewrite {label, lhs, rhs, weight}) =
        String.concat [ Atom.toString label, " : ", patternToString lhs,
                        " ==> ", patternToString rhs, " {",
                        IntInf.toString weight, "}" ]

    (* ____________________________________________________________ *)
    (* The following is based on the HLOp environment stuff.
       XXX - Not sure if this should just go in a separate module. *)

    datatype rw_env = RWEnv of { hlrwGrammar : grammar,
                                 tyEnv : BOMTy.ty AtomMap.map }

    fun emptyEnv () =
        RWEnv { hlrwGrammar = newGrammar (),
                tyEnv = AtomMap.empty }

    fun insertTy (RWEnv {hlrwGrammar, tyEnv}, id, ty) =
        RWEnv { hlrwGrammar = hlrwGrammar,
                tyEnv = AtomMap.insert(tyEnv, id, ty) }

    fun findTy (RWEnv {tyEnv, ...}, id) = 
        AtomMap.find(tyEnv, id)

    (* cvtTy() - Convert a parse tree type to its corresponding BOM type.

       XXX - This is essentially duplicate code from expand.sml.  Maybe this
       should/could be moved into the BOMTyPT structure? *)
    fun cvtTy (env, ty) = (case ty
        of PT.T_Any => BTy.T_Any
         | (PT.T_Enum w) => BTy.T_Enum w
         | (PT.T_Raw rty) => BTy.T_Raw rty
         | (PT.T_Tuple(mut, tys)) => BTy.T_Tuple(mut, cvtTys(env, tys))
         | (PT.T_Addr ty) => BTy.T_Addr(cvtTy(env, ty))
         | (PT.T_Fun(argTys, exhTys, resTys)) =>
           BTy.T_Fun(cvtTys(env, argTys), cvtTys(env, exhTys),
                     cvtTys(env, resTys))
         | (PT.T_Cont tys) => BTy.T_Cont(cvtTys(env, tys))
         | (PT.T_CFun cproto) => BTy.T_CFun cproto
         | (PT.T_VProc) => BTy.T_VProc
         | (PT.T_TyCon tyc) => (case findTy(env, tyc)
               of SOME ty => ty
                | NONE => (case BOMBasis.findTyc tyc
                      of SOME tyc => BTy.T_TyCon tyc
                       | NONE => raise (Fail ("unknown type: " ^
                                              (Atom.toString tyc)))
                      (* end case *))
               (* end case *))
        (* end case *))
    (* cvtTys() - Covert a list of parse tree types to a list of
       corresponding BOM types. *)
    and cvtTys (env, tys) = List.map (fn ty => cvtTy (env, ty)) tys

    fun addRWDefnToEnv (PT.RewriteDef (rw), RWEnv{hlrwGrammar, tyEnv}) =
        RWEnv { hlrwGrammar = addRWToGrammar(rw, hlrwGrammar),
                tyEnv = tyEnv }
      | addRWDefnToEnv (PT.TypeDef(id, ty), env as RWEnv{hlrwGrammar, tyEnv}) =
        insertTy(env, id, cvtTy(env, ty))

end (* Rewrites *)

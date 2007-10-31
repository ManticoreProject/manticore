(* rewrites.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Defines basics, including the rewrite grammar, of the HLOp rewrite
 * infrastructure.
 *)

structure Rewrites = struct

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

    (* addPatternToGrammar() - Add a rewrite pattern as a production
       in the grammar, returning the non-terminal and new grammar. *)
    fun addPatternToGrammar (pat, grammar, rw_opt) = let

        fun rwFolder (p, (alist, g)) = let
            val (prod_name, g') = addPatternToGrammar(p, g, NONE)
        in
            (prod_name :: alist, g')
        end (* rwFolder *)

    in case pat
        of HLRWDefPT.Call(fname, pats) => let
               val (argNTs, grammar') = foldl rwFolder ([], grammar) pats
               val prod_name = getNewNonterminal grammar'
               val prod = HLRWProduction { name = prod_name,
                                           rhs = (fname :: (rev argNTs)),
                                           rw_opt = rw_opt }
           in
               (prod_name, addProductionToGrammar (prod, grammar'))
           end
         | HLRWDefPT.Var(_) => (wildcard, grammar)
    end (* addPatternToGrammar() *)

    (* addRWToGrammar() - Convert a rewrite pattern to a set of
       productions, and use these to extend the given grammar. *)
    fun addRWToGrammar (rw as HLRWDefPT.Rewrite {lhs,...}, grammar : grammar) =
        #2 (addPatternToGrammar(lhs, grammar, SOME rw))

    (* rwOptToString() - Convert an optional rewrite to a label, weight pair.

       XXX Should this be moved to HLRWDefPT? *)
    fun rwOptToString (SOME (HLRWDefPT.Rewrite {label, weight, ...})) =
        String.concat ["{", IntInf.toString weight, ", ", Atom.toString label,
                       "}"]
      | rwOptToString NONE = "{0, -}"

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

end (* Rewrites *)

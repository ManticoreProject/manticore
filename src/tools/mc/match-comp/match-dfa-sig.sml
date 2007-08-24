(* match-dfa-sig.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *)

signature MATCH_DFA =
  sig

  (***** Paths *****
   *
   * Paths describe a location in a pattern.
   *)

    datatype path
      = ROOT of Var.var
      | PATH of {
	parent : path,		(* the path of the enclosing constructor *)
	index : int,		(* which child of the parent (0-based) *)
	ty : Types.ty		(* the type of the location *)
      }

  (* extend a path *)
    val extendPath : (path * int * Types.ty) -> path

  (* compare two paths *)
    val comparePath : (path * path) -> order

  (* the string representation of a path (for debugging) *)
    val pathToString : path -> string

  (* mapping from source variables to the path of their binding site *)
    type var_map = path Var.Map.map


  (***** DFA states *****)
    type dfa
    type state

    val same : (state * state) -> bool
    val compare : (state * state) -> order
    val hash : state -> word

    datatype state_kind
      = TEST of (path * (simple_pat * state) list)
      | BIND of (var_map * state)
      | FINAL of (bool * Var.Set.set * AST.exp)
		(* the boolean is true for "default" states *)
      | WHEN of (var_map * AST.exp * state * state)
      | ERROR

    and simple_pat
      = ANY
      | LIT of Literal.literal
      | CON of (AST.dcon * Types.ty list * path list)

  (* create a new DFA.  The variable is the argument
   * being tested by the DFA.
   *)
    val mkDFA : AST.var -> dfa

  (* return the number of states in a dfa *)
    val size : dfa -> int

  (* set the initial (root) state of the DFA *)
    val setInitialState : (dfa * state) -> unit

  (* get distinguished states from a DFA *)
    val initialState : dfa -> state
    val errorState : dfa -> state
    val finalStates : dfa -> state list

  (* get the argument of the DFA *)
    val getArg : dfa -> AST.var

  (* construct a test state *)
    val mkTest : (dfa * path * (simple_pat * state) list) -> state

  (* construct a variable binding state; the state argument should be
   * a final state.
   *)
    val mkBind : (dfa * var_map * state) -> state

  (* construct a final state *)
    val mkFinal : (dfa * bool * Var.Set.set * AST.exp) -> state

  (* construct a "when" test state; the first state argument should be
   * a final state.
   *)
    val mkWhen : (dfa * var_map * AST.exp * state * state) -> state

  (* return the kind of a state *)
    val kind : state -> state_kind

  (* return true if the state is an unused default state (rCount = 0) *)
    val unusedDefault : state -> bool

  (* return the reference count of a state *)
    val rCount : state -> int

  (* return the reference count of the error state *)
    val errorCount : dfa -> int

  (* path variables bound by a simple pattern *)
    val pathsOf : simple_pat -> path list

  (* return a string representation of a simple pattern *)
    val patToString : simple_pat -> string

  (* dump a DFA to a file *)
    val dump : TextIO.outstream * dfa -> unit

  end

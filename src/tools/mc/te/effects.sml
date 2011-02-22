(* effects.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Effects for the type-and-effect analysis.
 *)

structure Effects : sig

  type effect
  type effects

  val bottom : effects
  val top    : effects

  val isBottom : effects -> bool
  val isTop    : effects -> bool

  val addRaise : effects -> effects
  val addRead  : effects -> effects
  val addWrite : effects -> effects

  val deleteRaise : effects -> effects
  val deleteRead  : effects -> effects
  val deleteWrite : effects -> effects

  val mightRaise : effects -> bool
  val mightRead  : effects -> bool
  val mightWrite : effects -> bool

  val union : effects * effects -> effects

  val toString : effects -> string

end = struct

  structure Effect = struct
    
    datatype t
      = Raise
      | Read
      | Write

    local 
      fun toInt Raise = 0
	| toInt Read = 1
	| toInt Write = 2
    in
    (* compare : t * t -> order *)
    (* wlog uses alphabetical order *)
      fun compare (e1, e2) = Int.compare (toInt e1, toInt e2)
    end (* local *)
 
    fun toString Raise = "Raise"
      | toString Read = "Read"
      | toString Write = "Write"
   
    val all = [Raise, Read, Write]

  end

  structure E = Effect

  structure EffectSet = RedBlackSetFn(struct
				        type ord_key = E.t
					val compare = E.compare
				      end)
    
  type effect = E.t
  type effects = EffectSet.set

(* convenient bindings to operators in EffectSet *)
  val add     : effects * effect -> effects = EffectSet.add
  val delete  : effects * effect -> effects = EffectSet.delete
  val member  : effects * effect -> bool = EffectSet.member
  val union   : effects * effects -> effects = EffectSet.union

(* bottom is the empty set, top is the set of all effects *)
  val bottom : effects = EffectSet.empty
  val top    : effects = List.foldl EffectSet.add' bottom E.all

(* curried member functions *)
  val cmem  : effects -> effect -> bool = (fn s => fn x => member (s, x))
  val cmem' : effect -> effects -> bool = (fn x => fn s => member (s, x))

(* predicates to identify top and bottom *)
  val isBottom : effects -> bool = EffectSet.isEmpty
  val isTop    : effects -> bool = fn s => List.all (cmem s) E.all

(* curried add functions *)
  val cadd  : effects -> effect -> effects = (fn s => fn x => add (s, x))
  val cadd' : effect -> effects -> effects = (fn x => fn s => add (s, x))

(* curried delete functions *)
  val cdel  : effects -> effect -> effects = (fn s => fn x => delete (s, x))
  val cdel' : effect -> effects -> effects = (fn x => fn s => delete (s, x)) 

(* functions to add effects to effect sets *)
  val addRaise : effects -> effects = cadd' E.Raise
  val addRead  : effects -> effects = cadd' E.Read
  val addWrite : effects -> effects = cadd' E.Write

(* functions to remove effects from effect sets *)
  val deleteRaise : effects -> effects = cdel' E.Raise
  val deleteRead  : effects -> effects = cdel' E.Read
  val deleteWrite : effects -> effects = cdel' E.Write

(* functions to indicate presence of particular effects in sets *)
  val mightRaise : effects -> bool = cmem' E.Raise
  val mightRead  : effects -> bool = cmem' E.Read
  val mightWrite : effects -> bool = cmem' E.Write

(* toString produces strings like "{Raise,Write}" *)
  val toString : effects -> string = (fn fs => let
    val es = EffectSet.listItems fs
    val ss = List.map E.toString es
    in
      "{" ^ String.concatWith "," ss ^ "}"
    end)

end

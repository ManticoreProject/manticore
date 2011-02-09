(* effects.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Effects for the type-and-effect analysis.
 *)

structure Effects = struct

  structure Effect = struct
    
    datatype t
      = Exn
      | Read
      | Write

    local 
      fun toInt Exn = 0
	| toInt Read = 1
	| toInt Write = 2
    in
    (* compare : t * t -> order *)
    (* wlog uses alphabetical order *)
      fun compare (e1, e2) = Int.compare (toInt e1, toInt e2)
    end (* local *)
 
    fun toString Exn = "Exn"
      | toString Read = "Read"
      | toString Write = "Write"
   
    val all = [Exn, Read, Write]

  end

  structure EffectSet = RedBlackSetFn(struct
				        type ord_key = Effect.t
					val compare = Effect.compare
				      end)
    
  type effect = Effect.t
  type effects = EffectSet.set

  val bottom : effects = EffectSet.empty
  val top : effects = List.foldl EffectSet.add' bottom Effect.all

  val member : effects * effect -> bool = EffectSet.member
  val union : effects * effects -> effects = EffectSet.union
  val isEmpty : effects -> bool = EffectSet.isEmpty

(* curried member functions *)
  val cmem  : effects -> effect -> bool = (fn s => fn x => member (s, x))
  val cmem' : effect -> effects -> bool = (fn x => fn s => member (s, x))

  val isBottom : effects -> bool = isEmpty

  val isTop : effects -> bool = fn s => List.all (cmem s) Effect.all
    
  val mightRaise : effects -> bool = cmem' Effect.Exn

  val mightRead : effects -> bool = cmem' Effect.Read

  val mightWrite : effects -> bool = cmem' Effect.Write

  val toString : effects -> string = (fn fs => let
    val es = EffectSet.listItems fs
    val ss = List.map Effect.toString es
    in
      "{" ^ String.concatWith "," ss ^ "}"
    end)

end

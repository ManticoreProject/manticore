(* interface-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

(* The technical point here is that the notes/amsft paper
 * contains no account of type constructors (apart from perhaps
 * nullaries, in its non-specific reference to ground types).
 * So one needs to decide what to do with types like int option.
 *)

structure InterfaceTypes = struct

  datatype ty
    = ConTy   of ty list * tycon
    | FunTy   of ty * ty
    | TupleTy of ty list
    (* TySchemes, TyVars omitted for the time being *)

  and tycon 
    = Tyc of {
        stamp  : Stamp.stamp,
	name   : Atom.atom,
	arity  : int,
	params : ty list, (* NOTE no tyvars *)
	props  : PropList.holder,
	def    : tycon_def
      }

  and tycon_def 
    = AbsTyc
    | DataTyc of {
        nCons : int ref,
	cons  : dcon list ref
      }

  and dcon      
    = DCon of {
        id    : int,
	name  : Atom.atom,
	owner : tycon,
	argTy : ty option
      }

  fun same (t1, t2) = let
    fun ty (ConTy (ts1, c1), ConTy (ts2, c2)) = 
          ListPair.allEq ty (ts1, ts2) andalso tycon (c1, c2)
      | ty (FunTy (t1, u1), FunTy (t2, u2)) =
          ty (t1, t2) andalso ty (u1, u2)
      | ty (TupleTy ts1, TupleTy ts2) =
          ListPair.allEq ty (ts1, ts2)
      | ty _ = false
    and tycon (Tyc {stamp=s1,...}, Tyc {stamp=s2,...}) = 
          Stamp.same (s1, s2)
    in
      ty (t1, t2)
    end

  val toString : ty -> string = let
    fun ty (ConTy (ts, c)) =
         (case ts
	    of [] => tycon c
	     | _  => tycon c ^ tuple ts)
      | ty (FunTy (t, u)) = ty t ^ " -> " ^ ty u
      | ty (TupleTy ts) = tuple ts
    and tuple [] = "()"
      | tuple ts = "(" ^ (String.concatWith "," o List.map ty) ts ^ ")"
    and tycon (Tyc {stamp, name, arity, params, props, def}) =
          Atom.toString name ^ "<" ^ Stamp.toString stamp ^ ">"
    and dcon (DCon {id, name, owner, argTy}) =
         (case argTy
	    of NONE => Atom.toString name
	     | SOME t => ty t ^ " " ^ Atom.toString name)
    in
      ty
    end

end

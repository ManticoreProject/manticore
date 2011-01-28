(* representation-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure RepresentationTypes = struct

  structure NTy = NestingTreeTypes

  datatype ty
    = ConTy of (ty list * tycon)
    | FunTy of ty * ty
    | TupleTy of ty list
    | FlatArrayTy of ty * NTy.ty

  and tycon 
    = Tyc of {
        stamp : Stamp.stamp,
	name : Atom.atom,
	arity : int,
	params : Types.tyvar list, (* NOTE the only place tyvars occur *)
	props : PropList.holder,
	def : tycon_def
      }

  and tycon_def 
    = AbsTyc
    | DataTyc of {
        nCons : int ref,
	cons : dcon list ref
      }

  and dcon
    = DCon of {
        id : int,
	name : Atom.atom,
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
      | ty (FlatArrayTy (t1, n1), FlatArrayTy (t2, n2)) =
          ty (t1, t2) andalso NTy.same (n1, n2)
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
      | ty (FlatArrayTy (t, n)) = 
          "{" ^ ty t ^ " ; " ^ NTy.toString n ^ "}"
    and tuple [] = "()"
      | tuple ts = "(" ^ (String.concatWith "," o List.map ty) ts ^ ")"
    and tycon (Tyc {stamp, name, arity, params, props, def}) =
          Atom.toString name ^ Stamp.toString stamp
    and dcon (DCon {id, name, owner, argTy}) =
         (case argTy
	    of NONE => Atom.toString name
	     | SOME t => ty t ^ " " ^ Atom.toString name)
    in
      ty
    end


end

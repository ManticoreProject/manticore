(* ft-repr-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FTReprTypes = struct

  structure T = Types
  structure U = TypeUtil
  structure N = NestingTreeTypes

  datatype ty
    = VarTy of T.tyvar
    | ConTy of (ty list * tycon)
    | FunTy of ty * ty
    | TupleTy of ty list
    | FlatArrayTy of ty * N.ty (* ty is element type *)

  and tycon 
    = Tyc of {
        stamp : Stamp.stamp,
	name : Atom.atom,
	arity : int,
	params : Types.tyvar list,
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

  fun same (ConTy (ts1, c1), ConTy (ts2, c2)) = 
        ListPair.allEq same (ts1, ts2) andalso tycon (c1, c2)
    | same (FunTy (t1, u1), FunTy (t2, u2)) =
        same (t1, t2) andalso same (u1, u2)
    | same (TupleTy ts1, TupleTy ts2) =
        ListPair.allEq same (ts1, ts2)
    | same (FlatArrayTy (t1, n1), FlatArrayTy (t2, n2)) =
        same (t1, t2) andalso N.same (n1, n2)
    | same (VarTy a, VarTy b) =
        TyVar.same (a, b)
    | same _ = false
  and tycon (Tyc {stamp=s1, ...}, Tyc {stamp=s2, ...}) = Stamp.same (s1, s2)

  val toString : ty -> string = let
    fun repr (ConTy (ts, c)) =
         (case ts
	    of [] => tycon c
	     | [t] => repr t ^ " " ^ tycon c
	     | _  => tuple ts ^ " " ^ tycon c)
      | repr (FunTy (t, u)) = repr t ^ " -> " ^ repr u
      | repr (TupleTy ts) = tuple ts
      | repr (FlatArrayTy (t, n)) = 
          "{" ^ repr t ^ " ; " ^ N.toString n ^ "}"
      | repr (VarTy a) = TypeUtil.tyvarToString a
    and tuple [] = "()"
      | tuple ts = "(" ^ (String.concatWith "," o List.map repr) ts ^ ")"
    and tycon (Tyc {stamp, name, arity, params, props, def}) =
          Atom.toString name ^ Stamp.toString stamp
    and dcon (DCon {id, name, owner, argTy}) =
         (case argTy
	    of NONE => Atom.toString name
	     | SOME t => repr t ^ " " ^ Atom.toString name)
    in
      repr
    end

end

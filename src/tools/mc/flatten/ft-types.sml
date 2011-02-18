(* ft-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FTTypes = struct

  structure T = Types
  structure N = NestingTreeTypes
  structure U = TypeUtil

  datatype repr_ty
    = VarTy of Types.tyvar
    | ConTy of (repr_ty list * tycon)
    | FunTy of repr_ty * repr_ty
    | TupleTy of repr_ty list
    | FlatArrayTy of repr_ty * N.ty (* ty is element type *)

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
	argTy : repr_ty option
      }

  datatype ty
    = IR of T.ty * repr_ty

  fun same (t1 : ty, t2 : ty) : bool = let
    fun repr (ConTy (ts1, c1), ConTy (ts2, c2)) = 
          ListPair.allEq repr (ts1, ts2) andalso tycon (c1, c2)
      | repr (FunTy (t1, u1), FunTy (t2, u2)) =
          repr (t1, t2) andalso repr (u1, u2)
      | repr (TupleTy ts1, TupleTy ts2) =
          ListPair.allEq repr (ts1, ts2)
      | repr (FlatArrayTy (t1, n1), FlatArrayTy (t2, n2)) =
          repr (t1, t2) andalso N.same (n1, n2)
      | repr (VarTy a, VarTy b) =
          TyVar.same (a, b)
      | repr _ = false
    and tycon (Tyc {stamp=s1,...}, Tyc {stamp=s2,...}) = 
      Stamp.same (s1, s2)
    fun ty (IR (t1, r1), IR (t2, r2)) = 
      U.same (t1, t2) andalso repr (r1, r2)
    in
      ty (t1, t2)
    end

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
    fun ty (IR (t, r)) = 
      String.concat ["<", U.toString t, " / ", repr r, ">"]
    in
      ty
    end

  fun interfaceTy (IR (i, _)) = i

  fun reprTy (IR (_, r)) = r

end

(* realize-types.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure RealizeTypes (* : sig

  val realize : AST.ty -> AST.ty

end *) = struct

  structure A = AST
  structure T = Types 
  structure B = Basis

  structure AU = ASTUtil
  structure TU = TypeUtil

  structure DTbl = DataCon.Tbl
  structure TTbl = TyCon.Tbl

  fun ln () = TextIO.print "\n"
  fun println s = (TextIO.print s; ln ())

  fun assert (msg : string) (fact : bool) : unit = 
    if not fact then raise Fail ("assertion failure: " ^ msg) else ()

  val globTycTbl : T.tycon TTbl.hash_table  = TTbl.mkTable (32, Fail "tyc tbl")
  val findTyc : T.tycon -> T.tycon option   = TTbl.find globTycTbl
  val recordTyc : T.tycon * T.tycon -> unit = TTbl.insert globTycTbl

  val globDConTbl : T.dcon DTbl.hash_table = DTbl.mkTable (32, Fail "dcon tbl")
  val findDCon : T.dcon -> T.dcon option   = DTbl.find globDConTbl
  val recordDCon : T.dcon * T.dcon -> unit = DTbl.insert globDConTbl

(* TODO should clients of this code should have to ask if a type *)
(*   must be realized? Maybe I should change the interface. *)

  fun mustRealize (flatTycs : TyCon.Set.set) : T.ty -> bool = let
    fun tyc c = TyCon.Set.member (flatTycs, c)
    fun ty (T.ErrorTy) = false
      | ty (T.MetaTy (T.MVar {info, ...})) = (case !info
          of T.INSTANCE t' => ty t'
	   | _ => false
          (* end case *))
      | ty (T.VarTy a) = false
      | ty (T.ConTy (ts, c)) = List.exists ty ts orelse tyc c
      | ty (T.FunTy (t1, t2)) = ty t1 orelse ty t2
      | ty (T.TupleTy ts) = List.exists ty ts
      | ty (T.FArrayTy _) = true
    in
      ty
    end

  fun mkFunctions (flatTycs : TyCon.Set.set) = let
    fun flatTyc c = TyCon.Set.member (flatTycs, c)
    fun ty (T.ErrorTy) = T.ErrorTy
      | ty (m as T.MetaTy (T.MVar {info, ...})) = (case !info
          of T.INSTANCE t' => let
               val t'' = ty t'
               in
                 info := T.INSTANCE t'';
  	         m
  	       end
  	   | _ => m
          (* end case *))
      | ty (T.VarTy a) = T.VarTy a
      | ty (T.ConTy (ts, c)) = let
          val ts' = List.map ty ts
          val c' = if flatTyc c then tyc c else c
          in
  	    T.ConTy (ts', c')
          end
      | ty (T.FunTy (t1, t2)) = T.FunTy (ty t1, ty t2)
      | ty (T.TupleTy ts) = T.TupleTy (List.map ty ts)
      | ty (T.FArrayTy (t, n)) = T.ConTy ([ty t], BasisItems.farrayTyc ())
          (* note: we lose the shape tree type here *)
  and tyc c = (case findTyc c
    of SOME c' => c'
     | NONE => newTyc c
    (* end case *))
  and dcon d = (case findDCon d
    of SOME d' => d'
     | NONE => newDCon d
    (* end case *))
  and newDCon (d: T.dcon) : T.dcon = let
    val tyc = DataCon.ownerOf d
    val isNone = (fn NONE => true | SOME _ => false)
    val _ = assert "tycon not already realized" (isNone (findTyc tyc))
    val _ = assert "dcon should be realized" (flatTyc tyc)
    val tyc' = newTyc tyc
      (* creates all dcons too, and records them as side effect *)
    in case findDCon d
      of SOME d' => d'
       | NONE => raise Fail "compiler bug"
    end
  and newTyc (tyc : T.tycon) : T.tycon = let
    val _ = assert "data tyc" (TU.isDataTyc tyc)
    val _  = assert "tyc should in fact be realized" (flatTyc tyc)
    val (T.Tyc {name, params, def, ...}) = tyc
    val cons = (case def
      of T.DataTyc {cons, ...} => !cons
       | _ => raise Fail "newTyc"
      (* end case *))
    val name' = Atom.atom (Atom.toString name ^ "f")
    val tyc' = TyCon.newDataTyc (name', params)
    fun dcon d = let
      val name = DataCon.nameOf d
      val name' = Atom.atom (name ^ "f")
      val argTy = DataCon.argTypeOf d
      val argTy' = Option.map ty argTy
      val d' = DataCon.new tyc' (name', argTy')
        (* note: this registers d' with tyc' as a side effect *)
      in
        recordDCon (d, d')
      end
    in
      List.app dcon cons;
      recordTyc (tyc, tyc');
      tyc'
    end
  fun scheme (T.TyScheme (xs, t)) = T.TyScheme (xs, ty t)
  val flop = let
    fun rt (A.ID t) = A.ID (ty t)
      | rt (A.Unzip t) = A.Unzip (ty t)
      | rt (A.Cat t) = A.Cat (ty t)
      | rt (A.Map (op1, n)) = A.Map (rt op1, n)
      | rt (A.Compose (op1, op2)) = A.Compose (rt op1, rt op2)
      | rt (A.CrossCompose ops) = A.CrossCompose (List.map rt ops)
    in
      rt
    end
  val pop = let
    fun ps (A.PSub_Nested t) = A.PSub_Nested (ty t)
      | ps (A.PSub_Flat t) = A.PSub_Flat (ty t)
      | ps (A.PSub_Tuple os) = A.PSub_Tuple (List.map ps os)
    fun pop (A.PA_Length t) = A.PA_Length (ty t)
      | pop (A.PA_Sub s) = A.PA_Sub (ps s)
      | pop (A.PA_TabFromToStep t) = A.PA_TabFromToStep (ty t)
      | pop (A.PA_Map t) = A.PA_Map (ty t)
      | pop (A.PA_Reduce t) = A.PA_Reduce (ty t)
      | pop (A.PA_Range t) = A.PA_Range (ty t)
    in
      pop
    end
  in
    {realizeTy = ty,
     realizeTyc = tyc,
     realizeDCon = dcon,
     realizeScheme = scheme,
     realizeFlOp = flop,
     realizePop = pop}
  end

end

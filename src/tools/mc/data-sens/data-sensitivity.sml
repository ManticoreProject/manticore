(* data-sensitivity.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure DataSensitivity = struct

  structure A = AST
  structure B = Basis
  structure T = Types

  structure AU = ASTUtil
  structure TU = TypeUtil

  structure VT = Var.Tbl 

  fun todo () = raise Fail "todo"

  datatype sensitivity
    = DataSensitive
    | DataInsensitive

(* short-circuiting sensitivity check over lists *)
  val $ f = let
    fun lp [] = DataInsensitive
      | lp (x::xs) = (case f x
          of DataSensitive => DataSensitive
	   | DataInsensitive => lp xs
          (* end case *))
    in
      lp
    end

(* thunking *)
  fun `  f x   = fn () => f x
  fun `` f x y = fn () => f x y
  fun `$ f xs  = fn () => $f xs

(* short-circuiting monadic bindSeq *)
  infix >>= 5
  fun s >>= k = (case s
    of DataSensitive => DataSensitive
     | DataInsensitive => k ()
    (* end case *))

  val env : sensitivity VT.hash_table

  fun mkEnv () = let
    val env : env = VT.mkTable (256, Fail "DataSensitivity.env")
    val addAll = List.app (fn x => VT.insert env (x, DataInsensitive))
    in
      List.app addAll [B.intOpers, B.longOpers, B.integerOpers, 
		       B.floatOpers, B.doubleOpers, B.charOpers, 
		       B.runeOpers, B.stringOpers];
      env
    end

  fun analyze (e : A.exp) : env = let
    val env = mkEnv ()
    fun exp e = (case e
      of A.LetExp (b, e) => binding b >>= `exp e
       | A.IfExp (e1, e2, e3, t) => exp e1 >>= `exp e2 >>= `exp e3
       | A.CaseExp (e, ms, t) => exp e >>= `$match ms
       | A.PCaseExp (es, ms, t) => $exp es >>= `$match ms
       | A.HandleExp (e, ms, t) => exp e >>= `$match ms
       | A.RaiseExp (e, t) => exp e
       | A.FunExp (x, e, t) => exp e
       | A.ApplyExp (e1, e2, t) => (case e1
           of A.VarExp (f, ts) => (case VT.find env f
                of SOME s => s >>= `exp e2
		 | NONE => DataSensitive
                (* end case *))
	    | _ => exp e1 >>= `exp e2
       | A.VarArityOpExp _ => DataInsensitive
       | A.TupleExp es => $exp es
       | A.RangeExp (e1, e2, o3, t) => exp e1 >>= `exp e2 >>= ``Option.app exp o3
       | A.PTupleExp es => $exp es
       | A.PArrayExp (es, t) => $exp es
       | A.PCompExp (e, pes, opt) => 
           exp e >>= `$(exp o #2) pes >>= ``Option.app exp opt
       | A.PChoiceExp (es, t) => $exp es
       | A.SpawnExp e => exp e
       | A.ConstExp _ => DataInsensitive
       | A.VarExp (x, ts) => (case VT.find env x
           of SOME s => s
	    | NONE => DataInsensitive
           (* end case *))
       | A.SeqExp (e1, e2) => exp e1 >>= `exp e2
       | A.OverloadExp _ => DataInsensitive
       | A.ExpansionOptsExp (opts, e) => exp e
       | A.PArrayOp _ => DataInsensitive
       | A.FTupleExp es => $exp es
       | A.FArrayExp (es, n, t) => $exp es >>= `ntree n
       | A.FlOp _ => DataInsensitive
      (* end case *))
    and ntree n = (case n
      of A.Lf (e1, e2) => exp e1 >>= `exp e2
       | A.Nd ns => $ntree ns
      (* end case *))
    and binding b = (case b
      of A.ValBind (p, e) => exp e
       | A.PValBind (p, e) => exp e
       | A.FunBind lams => $lambda lams
       | A.PrimVBind _ => ()
       | A.PrimCodeBind _ => ()
      (* end case *))
    and match m = (case m
      of A.PatMatch (p, e) => exp e
       | A.CondMatch (p, e1, e2) => exp e1 >>= `exp e2
      (* end case *))
    and lambda fb = (case fb
      of A.FB (f, x, e) => VT.insert env (f, exp e)
      (* end case *))
    in
      (exp e; 
       env)
    end

  fun transform (e : A.exp) : A.exp = todo ()

end

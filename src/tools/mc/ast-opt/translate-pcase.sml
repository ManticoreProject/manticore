(* translate-pcase.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslatePCase (* : sig

  (* An AST to AST translation of parallel cases. *)
    val tr : (AST.exp -> AST.exp) 
             -> AST.exp list * AST.pmatch list * AST.ty 
             -> AST.exp

  end *) = struct

    structure A = AST
    structure B = Basis
    structure F = Futures
    structure R = Ropes
    structure U = UnseenBasis

    structure CompletionBitstring : sig
      type t
      val eq  : t * t -> bool
      val sub : t * t -> bool
      val toString : t -> string
      val fromPPats : A.ppat list -> t
      val allOnes : int -> t
    end = struct
      datatype bit = Zero | One
      type t = bit list
      fun bitEq (b1:bit, b2:bit) = (b1=b2)
      fun eq (c1, c2) = 
            if (List.length c1) <> (List.length c2) then
              raise Fail "UnequalLengths"
	    else ListPair.all bitEq (c1, c2)
      (* c1 < c2 if everywhere c1 is 1, c2 is 1. *)
      (* If one thinks of c1 and c2 as bit-vector sets, this is the subset relationship. *)
      fun sub (c1, c2) = let
        fun s ([], []) = true
	  | s (One::t1, One::t2) = s (t1, t2)
	  | s (Zero::t1,  _::t2) = s (t1, t2)
	  | s _ =  raise Fail "bug" (* unequal length lists screened out below*)
        in
	  if (length c1) <> (length c2) then
	    raise Fail "UnequalLengths"
	  else s (c1, c2)
        end
      fun toString cb = concat (map (fn Zero => "0" | One => "1") cb)
      fun fromPPats ps = map (fn A.NDWildPat _ => Zero | _ => One) ps
      fun allOnes n = List.tabulate (n, fn _ => One) 
    end

    structure CB = CompletionBitstring

    (* PCaseExp of (exp list * pmatch list * ty)       (* ty is result type *) *)   

    fun tr trExp (es, pms, t) = let
      val nExps = List.length es
      fun mkCBs (A.Otherwise _ :: [], acc) = rev (CB.allOnes(nExps)::acc)
	| mkCBs (A.PMatch _ ::[], _) = raise Fail "ill-formed pcase: otherwise is not last"
	| mkCBs (A.Otherwise _ :: t, _) = raise Fail "ill-formed pcase: otherwise is not last"
	| mkCBs (A.PMatch (ps, _) :: t, acc) = mkCBs (t, CB.fromPPats(ps)::acc)  
	| mkCBs ([], _) = raise Fail "ill-formed pcase: no branches"
      (* Is there a check somewhere to ensure the "arities" of all pms is same as nExps? *)
      val cbs = mkCBs (pms, [])
      in
        raise Fail "todo"
      end

  end

(* flatten-env.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure FlattenEnv : sig

    type env

    val mkEnv : unit -> env

    datatype var_bind
      = Var of FLAST.var
      | EqOp

    val insertTyc	: (env * Types.tycon * FTTypes.tycon) -> unit
    val insertCon	: (env * Types.dcon * FTTypes.dcon)   -> unit
    val insertVar	: (env * AST.var * FLAST.var)         -> env

    val findTyc		: (env * Types.tycon) -> FTTypes.tycon option
    val findDCon	: (env * Types.dcon)  -> FTTypes.dcon option
    val lookupVar	: (env * AST.var)     -> var_bind

  (* output an environment *)
    val dump : (TextIO.outstream * env) -> unit

  end = struct

    structure TTbl = TyCon.Tbl
    structure DTbl = DataCon.Tbl
    structure VMap = Var.Map
    structure ATbl = AtomTable

    datatype var_bind
      = Var of FLAST.var
      | EqOp

    datatype env = E of {
	tycEnv  : FTTypes.tycon TTbl.hash_table,
	dconEnv : FTTypes.dcon DTbl.hash_table,
	varEnv  : var_bind VMap.map
      }

    local
      val bs = [(Basis.eq, EqOp), (Basis.neq, EqOp)]
    in
      fun mkEnv () = 
        E {tycEnv  = TTbl.mkTable (32, Fail "tyc table"),
	   dconEnv = DTbl.mkTable (64, Fail "dcon table"),
	   varEnv  = List.foldl VMap.insert' VMap.empty bs}
    end (* local *)

    fun insertTyc (E {tycEnv, ...}, aTyc, fTyc) = 
      TTbl.insert tycEnv (aTyc, fTyc)

    fun insertCon (E {dconEnv, ...}, aCon, fCon) = 
      DTbl.insert dconEnv (aCon, fCon)

    fun insertVar (E {tycEnv, dconEnv, varEnv}, x, fx) =
      E {tycEnv  = tycEnv,
	 dconEnv = dconEnv,
	 varEnv  = VMap.insert (varEnv, x, Var fx)}

    fun findTyc (E {tycEnv, ...}, tyc) = TTbl.find tycEnv tyc

    fun findDCon (E {dconEnv, ...}, dc) = DTbl.find dconEnv dc

    fun lookupVar (E {varEnv, ...}, x) =  
     (case VMap.find (varEnv, x)
        of SOME fx => fx
	 | NONE => raise Fail(concat["lookupVar(_, ", Var.toString x, ")"])
       (* end case *))

  (* output an environment *)
    fun dump (outStrm, E{tycEnv, dconEnv, varEnv, ...}) = let
	  fun pr x = TextIO.output (outStrm, x)
	  fun prl xs = pr (String.concat xs)
	  fun prTyc (aTyc, fTyc) = prl [
		  "    ", TyCon.toString aTyc, "  :->  ", 
		  FTTyCon.toString fTyc, "\n"
		]
	  fun prDcon (aCon, fCon) = prl [
		  "    ", DataCon.toString aCon, "  :->  ", 
		  FTDataCon.toString fCon, "\n"
		]
	  fun prVar (x, bind) = let
		val bind = (case bind
		      of Var x' => FTVar.toString x'
		       | EqOp => "<eq>"
		     (* end case *))
	        in
		  prl ["    ", Var.toString x, " : ", 
		       TypeUtil.schemeToString(Var.typeOf x), "  :->  ", 
		       bind, "\n"]
	        end
	  in
	    pr "***** Flatten translation environment dump *****\n";
	    pr "  *** Type constructors:\n";
	    TTbl.appi prTyc tycEnv;
	    pr "  *** Data constructors:\n";
	    DTbl.appi prDcon dconEnv;
	    pr "  *** Variables:\n";
	    VMap.appi prVar varEnv;
	    pr "*****\n"
	  end

  end

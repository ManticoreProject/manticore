(* check-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check a CFG module for well-formedness
 *
 * TODO:
 *	Check that T_Addr does not appear in tuple or open-tuple types.
 *)

structure CheckCFG : sig

    val check : string * CFG.module -> bool

  end = struct

    structure V = CFG.Var
    structure L = CFG.Label
    structure VSet = CFG.Var.Set
    structure LSet = CFG.Label.Set
    structure LMap = CFG.Label.Map
    structure Ty = CFGTy
    structure TyU = CFGTyUtil
    structure Lit = Literal

    val v2s = V.toString
    fun vl2s xs = String.concat["(", String.concatWith "," (List.map v2s xs), ")"]
    fun v2s' x = concat[v2s x, ":", TyU.toString(V.typeOf x)]
    fun vl2s' xs = String.concat["(", String.concatWith "," (List.map v2s' xs), ")"]

    val l2s = L.toString
    fun l2s' l = concat[l2s l, ":", TyU.toString(L.typeOf l)]

    val t2s = TyU.toString
    fun tl2s ts = concat["(", String.concatWith "," (map t2s ts), ")"]

  (* placeholder for testing variable kind equality *)
    fun eqVK _ = true

    fun vkToString CFG.VK_None = "VK_None"
      | vkToString (CFG.VK_Let _) =
	  concat["VK_Let(", "_", ")"]
      | vkToString (CFG.VK_Param _) = "VK_Param"

    fun typesOf xs = List.map V.typeOf xs

    fun check (phase, module) = let
          val CFG.MODULE{name, externs, code} = module
	  val anyErrors = ref false
	(* report an error *)
	  fun pr s = TextIO.output(TextIO.stdErr, concat s)
	  fun error msg = (
		if !anyErrors then ()
		else (
		  pr ["***** Bogus CFG in ", Atom.toString name, " after ", phase, " *****\n"];
		  anyErrors := true);
		pr ("** " :: msg))
	  fun cerror msg = pr ("== "::msg)
	(* match the parameter types against argument variables *)
        (* checkArgTypes : string * ty list * ty list -> unit *)
	  fun checkArgTypes (cmp, ctx, paramTys, argTys) = let
	      (* chk1 : ty * ty -> unit *)
	        fun chk1 (pty, aty) =
		      if (cmp (aty, pty))
                        then ()
		        else (
			  error ["type mismatch in ", ctx, "\n"];
			  cerror ["  expected  ", TyU.toString pty, "\n"];
			  cerror ["  but found ", TyU.toString aty, "\n"])
	        in 
	          if (length paramTys = length argTys)
                    then ListPair.app chk1 (paramTys, argTys)
                    else let
	            (* str : ty list -> string *)
                      fun str ts = String.concatWith "," (map TyU.toString ts)
                      in 
                        error ["wrong number of arguments in ", ctx, "\n"];
			cerror ["  expected (", str paramTys, ")\n"];
			cerror ["  found    (", str argTys, ")\n"]
                      end
	        end
	(* construct a set of the bound labels in the module *)
	  val lEnv = List.foldl
		(fn (f as CFG.FUNC{lab, ...}, lset) => LSet.add(lset, lab))
		  LSet.empty code
	(* Check that a variable is bound *)
          fun addVar (env, x) = VSet.add(env, x)
          fun addVars (env, xs) = VSet.addList(env, xs)
	  fun chkVar (env, x, cxt) = if VSet.member(env, x)
		then ()
		else error["unbound variable ", v2s x, " in ", cxt, "\n"]
	  fun chkVars (env, xs, cxt) = List.app (fn x => chkVar(env, x, cxt)) xs
	  fun chkBinding (x, binding) = if eqVK(V.kindOf x, binding)
		then ()
		else error[
		    "binding of ", v2s x, " is ",
		    vkToString(V.kindOf x), " (expected ",
		    vkToString binding, ")\n"
		  ]
	  fun chkBindings (lhs, binding) =
		List.app (fn x => chkBinding(x, binding)) lhs
          fun chkLbl (env, l, cxt) = (case (L.kindOf l, LSet.member(env, l))
                 of (CFG.LK_None, _) => error["no kind label ", l2s l, " in ", cxt, "\n"]
                  | (CFG.LK_Extern _, false) => ()
                  | (CFG.LK_Extern _, true) => error["extern local label ", l2s l, " in ", cxt, "\n"]
                  | (CFG.LK_Local _, true) => ()
                  | (CFG.LK_Local _, false) => error["unbound label ", l2s l, " in ", cxt, "\n"]
                (* end case *))
	(* check the entry against the declared type of the label;  The declared type is 
         * allowed to be more specific.
         *)
          fun chkEntry (lab, entry) = (case (entry, L.typeOf lab) 
                 of (CFG.StdFunc {clos, args, ret, exh},
                     Ty.T_StdFun {clos = closTy, args = argTys, ret = retTy, exh = exhTy}) => (
                      (* FIXME: closure types not set by assignLabels *)
                      checkArgTypes(TyU.match, concat["StdFun ", l2s lab, " clos"],
                                    [closTy], typesOf [clos]);
                      checkArgTypes(TyU.equal, concat["StdFun ", l2s lab, " args"],
                                    argTys, typesOf args);
                      checkArgTypes(TyU.equal, concat["StdFun ", l2s lab, " ret"],
                                    [retTy], typesOf [ret]);
                      checkArgTypes(TyU.equal, concat["StdFun ", l2s lab, " exh"],
                                    [exhTy], typesOf [exh]);
                      addVars(VSet.empty, clos::ret::exh::args))
                  | (CFG.StdCont {clos, args},
                     Ty.T_StdCont {clos = closTy, args = argTys}) => (
                      (* FIXME: closure types not set by assignLabels *)
                      checkArgTypes(TyU.match, concat["StdCont ", l2s lab, " clos"],
                                    [closTy], typesOf [clos]);
                      checkArgTypes(TyU.equal, concat["StdCont ", l2s lab, " args"],
                                    argTys, typesOf args);
                      addVars(VSet.empty, clos::args))
                  | (CFG.KnownFunc {clos, args}, 
                     Ty.T_KnownFunc {clos = closTy, args = argTys}) => (
                      (* FIXME: closure types not set by assignLabels *)
                      checkArgTypes(TyU.match, concat["KnownFunc ", l2s lab, " clos"],
                                    [closTy], typesOf [clos]);
                      checkArgTypes(TyU.equal, concat["KnownFunc ", l2s lab, " args"],
                                    argTys, typesOf args);
                      addVars(VSet.empty, clos::args))
                  | (CFG.Block {args}, Ty.T_Block {args = argTys}) => (
                      checkArgTypes(TyU.equal, concat["Block ", l2s lab, " args"],
                                    argTys, typesOf args);
                      addVars(VSet.empty, args))
                  | (conv, ty) => (
                      error["entry of ", l2s lab, " is ", 
                            (case conv 
                              of CFG.StdFunc _ => "stdfunc"
                               | CFG.StdCont _ => "stdcont"
                               | CFG.KnownFunc _ => "known"
                               | CFG.Block _ => "block"
                             (* end case *)), 
                            " (expected ", TyU.toString ty, ")\n"];
                      addVars(VSet.empty, CFG.paramsOfConv conv))
                (* end case *))
          fun chkExp (env, exp) = (case exp 
                 of CFG.E_Var (xs, ys) => (
                      chkVars (env, ys, "Var");
                      checkArgTypes (TyU.equal, "Var", typesOf xs, typesOf ys);
                      addVars (env, xs))
                  | CFG.E_Const (x, lit) => (let
                      fun err () = error[
                             "type mismatch in Const: ", v2s' x, " = ",
                             Literal.toString lit, "\n"]
                      in 
                      case (V.typeOf x, lit)
                       of (Ty.T_Enum wt, Lit.Enum w) => if Word.<= (w, wt) 
                            then ()
                            else err ()
                        | _ => ()
                      (* end case *);
                      addVar (env, x)
                      end)
                  | CFG.E_Cast (x, ty', y) => (let
                      fun err () = error[
                             "type mismatch in Cast: ", v2s' x, " = ",
                             "(", TyU.toString ty', ")(", v2s' y, ")\n"];
                      in
                      chkVar (env, y, "Cast");
                      if TyU.match(ty', V.typeOf x) andalso TyU.validCast (V.typeOf y, ty')
                        then ()
                        else err ();
                      addVar (env, x)
                      end)
                  | CFG.E_Label (x, l) => (let
                      fun err () = error[
                             "type mismatch in Label: ", v2s' x, " = ", l2s' l, "\n"]
                      in
                      chkLbl (lEnv, l, "Label");
                      case L.kindOf l
                       of CFG.LK_None => error["no kind label ", l2s l, " in Label\n"]
                        | CFG.LK_Extern _ => ()
			| CFG.LK_Local _ => if TyU.equal (V.typeOf x, L.typeOf l)
                            then ()
                            else err ()
                      (* end case *);
                      addVar (env, x)
                      end)
                  | CFG.E_Select (x, i, y) => (let
                      fun err () = error[
                             "type mismatch in Select: ", v2s' x, " = ",
                             "#", Int.toString i, "(", v2s' y, ")\n"]
                      in
                      chkVar (env, y, "Select");
                      
                      case V.typeOf y
                       of Ty.T_Tuple(_, tys) =>
                            if (i < List.length tys) andalso TyU.match (List.nth (tys, i), V.typeOf x)
                              then ()
                              else err ()
                        | Ty.T_OpenTuple(tys) =>
                            if (i < List.length tys) andalso TyU.match (List.nth (tys, i), V.typeOf x)
                              then ()
                              else err ()
                        | _ => err ()
                      (* end case *);
                      addVar (env, x)
                      end)
                  | CFG.E_Update (i, y, z) => (let
                      fun err () = error[
                             "type mismatch in Update: ",
                             "#", Int.toString i, "(", v2s' y, ") := ", v2s' z, "\n"]
                      in
                      chkVar (env, y, "Update");
                      chkVar (env, z, "Update");
                      case V.typeOf y
                       of Ty.T_Tuple(true, tys) => 
			    if TyU.equal (V.typeOf z, List.nth (tys, i))
			      then ()
			      else err ()
			| ty => err ()
		      (* end case *);
                      env
                      end)
                  | CFG.E_AddrOf (x, i, y) => (let
                      fun err () = error [
                             "type mismatch in AddrOf: ", v2s' x, " = ",
                             "&(", v2s' y, ")\n"]
                      in
                      chkVar (env, y, "AddrOf");
                      case V.typeOf y
                       of Ty.T_Tuple(_, tys) =>
                            if (i < List.length tys) andalso TyU.match (Ty.T_Addr(List.nth (tys, i)), V.typeOf x)
                              then ()
                              else err ()
                        | _ => err ()
                      (* end case *);
                      addVar (env, x)
                      end)
                  | CFG.E_Alloc (x, ys) => (let
                      fun err () = error[
                             "type mismatch in Alloc: ", v2s' x, " = ",
                             "alloc(", vl2s' ys, ")\n"]
                      in
                      chkVars (env, ys, "Alloc");
                      if (TyU.match (Ty.T_Tuple (true, typesOf ys), V.typeOf x))
                        orelse (TyU.match (Ty.T_Tuple (false, typesOf ys), V.typeOf x))
                        then ()
                        else err ();
                      addVar (env, x)
                      end)
                  | CFG.E_GAlloc (x, ys) => (let
                      fun err () = error[
                             "type mismatch in GAlloc: ", v2s' x, " = ",
                             "galloc(", vl2s' ys, ")\n"]
                      in
                      chkVars (env, ys, "GAlloc");
                      if (TyU.match (Ty.T_Tuple (true, typesOf ys), V.typeOf x))
                        orelse (TyU.match (Ty.T_Tuple (false, typesOf ys), V.typeOf x))
                        then ()
                        else err ();
                      addVar (env, x)
                      end)
                  | CFG.E_Promote (x, y) => (let
                      fun err () = error[
                             "type mismatch in Promote: ", v2s' x, " = ",
                             "promote(", v2s' y, ")\n"]
                      in
                      chkVar (env, y, "Promote");
                      if TyU.equal (V.typeOf x, V.typeOf y)
                        then ()
                        else err ();
                      addVar (env, x)
                      end)
                  | CFG.E_Prim (x, p) => (
                      chkVars (env, PrimUtil.varsOf p, PrimUtil.nameOf p);
                      addVar (env, x))
                  | CFG.E_CCall (xs, cf, args) => (
                      chkVar (env, cf, "CCall");
                      chkVars (env, args, "CCall args");
                      addVars (env, xs))
                  | CFG.E_HostVProc x => (let
                      fun err () = error[
                             "type mismatch in HostVProc: ", v2s' x, " = ",
                             "host_vproc()\n"]
                      in
                      if TyU.match (Ty.T_VProc, V.typeOf x)
                        then ()
                        else err ();
                      addVar (env, x)
                      end)
                  | CFG.E_VPLoad (x, i, y) => (let
                      fun err () = error[
                             "type mismatch in VProcLoad: ", v2s' x, " = ",
                             "load(", v2s' y, "+", IntInf.toString i, ")\n"]
                      in
                      chkVar (env, y, "VPLoad");
                      if TyU.equal (Ty.T_VProc, V.typeOf y)
                        then ()
                        else err ();
                      addVar (env, x)
                      end)
                  | CFG.E_VPStore (i, y, z) => (let
                      fun err () = error[
                             "type mismatch in VProcStore: ",
                             "store(", v2s' y, "+", IntInf.toString i, ", ", v2s' z, ")\n"]
                      in
                      chkVar (env, y, "VPStore");
                      chkVar (env, z, "VPStore");
                      if TyU.equal (Ty.T_VProc, V.typeOf y)
                        then ()
                        else err ();
                      env
                      end)
                (* end case *))
          fun chkExit (env, exit) = (case exit
                 of CFG.StdApply{f, clos, args, ret, exh} => (
		      chkVar (env, f, "StdApply");
                      case V.typeOf f
                       of Ty.T_StdFun{clos = closTy, args = argTys, ret = retTy, exh = exhTy} => (
                            chkVar (env, clos, "StdApply clos");
                            chkVars (env, args, "StdApply args");
                            chkVar (env, ret, "StdApply ret");
                            chkVar (env, exh, "StdApply exh");
                            checkArgTypes (TyU.match, concat ["StdApply ", v2s f, " clos"], 
                                           [closTy], typesOf [clos]);
                            checkArgTypes (TyU.match, concat ["StdApply ", v2s f, " args"], 
                                           argTys, typesOf args);
                            checkArgTypes (TyU.match, concat ["StdApply ", v2s f, " ret"], 
                                           [retTy], typesOf [ret]);
                            checkArgTypes (TyU.match, concat ["StdApply ", v2s f, " exh"], 
                                           [exhTy], typesOf [exh]))
                        | ty => error[v2s f, ":", TyU.toString ty, " is not a stdfun\n"]
                      (* end case *))
                  | CFG.StdThrow{k, clos, args} => (
                      chkVar (env, k, "StdThrow");
                      case V.typeOf k
                       of Ty.T_StdCont{clos = closTy, args = argTys} => (
                            chkVar (env, clos, "StdThrow clos");
                            chkVars (env, args, "StdThrow args");
                            checkArgTypes (TyU.match, concat ["StdThrow ", v2s k, " clos"], 
                                           [closTy], typesOf [clos]);
                            checkArgTypes (TyU.match, concat ["StdThrow ", v2s k, " args"], 
                                           argTys, typesOf args))
                        | ty => error[v2s k, ":", TyU.toString ty, " is not a stdcont\n"]
                      (* end case *))
                  | CFG.Apply{f, clos, args} => (
                      chkVar (env, f, "Apply");
                      case V.typeOf f 
                       of Ty.T_KnownFunc {clos = closTy, args = argTys} => (
                            chkVar (env, clos, "Apply clos");
                            chkVars (env, args, "Apply args");
                            checkArgTypes (TyU.match, concat ["Apply ", v2s f, " clos"], 
                                           [closTy], typesOf [clos]);
                            checkArgTypes (TyU.match, concat ["Apply ", v2s f, " args"], 
                                           argTys, typesOf args))
                        | ty => error[v2s f, ":", TyU.toString ty, "is not a known\n"]
                      (* end case *))
                  | CFG.Goto jmp => chkJump (env, jmp, "Goto")
                  | CFG.If(x, j1, j2) => (
                      chkVar (env, x, "If");
                      if TyU.equal (V.typeOf x, Ty.boolTy)
                         then ()
                         else (
                           error ["type mismatch in If condition\n"];
                           cerror ["  expected  ", TyU.toString Ty.boolTy, "\n"];
                           cerror ["  but found ", TyU.toString (V.typeOf x), "\n"]);
                      chkJump (env, j1, "If/true");
                      chkJump (env, j2, "If/false"))
                  | CFG.Switch(x, cases, dflt) => (
                      chkVar (env, x, "Switch");
                      case V.typeOf x
                       of Ty.T_Enum wt => let
                            fun chkCase (tag, jmp) = (
			           if (tag <= wt)
                                      then ()
                                      else (
                                        error ["case out of range for Switch\n"];
                                        cerror ["  expected  ", TyU.toString (V.typeOf x), "\n"];
                                        cerror ["  but found ", Word.toString tag, "\n"]);
                                   chkJump (env, jmp, "Switch/case"))
                            in
                              List.app chkCase cases;
                              Option.app (fn j => chkJump (env, j, "Switch/dflt")) dflt
                            end
                        | Ty.T_Raw rt => let
                            fun chkCase (tag, jmp) = 
                                   chkJump(env, jmp, "Switch/case")
                            fun chk () = (
                                   List.app chkCase cases; 
                                   Option.app (fn j => chkJump (env, j, "Switch/dflt")) dflt)
                            fun bad () = (
                                   error ["type mismatch in Switch argument\n"];
                                   cerror ["  but found ", TyU.toString (V.typeOf x), "\n"])
                            in
                              case rt
                               of RawTypes.T_Byte => chk ()
                                | RawTypes.T_Short => chk ()
                                | RawTypes.T_Int => chk ()
                                | RawTypes.T_Long => chk ()
                                | RawTypes.T_Float => bad ()
                                | RawTypes.T_Double => bad ()
                                | RawTypes.T_Vec128 => bad ()
                            end
                        | _ => (
                            error ["type mismatch in Switch argument\n"];
                            cerror ["  expected  ", "enum or raw", "\n"];
                            cerror ["  but found ", TyU.toString (V.typeOf x), "\n"])
		      (* end case *))
		  | CFG.HeapCheck{hck, szb, nogc} => (
                      chkJump (env, nogc, "HeapCheck"))
                  | CFG.AllocCCall{lhs, f, args, ret = (l,rargs)} => (
                       chkVar (env, f, "AllocCCall");
                       chkVars (env, args, "AllocCCall");
                       case V.typeOf f
                        of Ty.T_CFun (CFunctions.CProto (retTy, argTys, _)) => ()
                         | ty => error[v2s f, ":", TyU.toString ty, "is not a cfun\n"]
                       (* end case *);
                       chkJump (addVars (env, lhs), (l,lhs@rargs), "AllocCCall"))
                (* end case *))
		and chkJump (env, (lab, args), cxt) = (
		      chkLbl (lEnv, lab, cxt);
		      chkVars (env, args, cxt);
                      case L.typeOf lab 
                       of Ty.T_Block {args = argTys} => (
                           checkArgTypes (TyU.equal, cxt, argTys, typesOf args))
                        | ty => error[l2s lab, ":", TyU.toString ty, " is not a block\n"])
          fun chkFunc (CFG.FUNC {lab, entry, body, exit}) = let
                val env = chkEntry (lab, entry)
		val env = List.foldl (fn (exp,env) => chkExp (env, exp)) env body
                val _ = chkExit (env, exit)
                in
                   ()
                end
          in
            List.app chkFunc code;
if !anyErrors
  then (
    print "******************** broken CFG ********************\n";
    PrintCFG.print module;
    print "********************\n";
    raise Fail "broken CFG")
  else ();
	  (* return the error status *)
	    !anyErrors
	  end (* check *)


          (* ******************************************** *)

(*
    structure V = CFG.Var
    structure L = CFG.Label
    structure VSet = CFG.Var.Set
    structure LSet = CFG.Label.Set
    structure LMap = CFG.Label.Map
    structure Ty = CFGTy
    structure TyU = CFGTyUtil
    structure Lit = Literal

    fun error msg = TextIO.output(TextIO.stdErr, concat("CFG Error: " :: msg @ ["\n"]))

    fun warning msg = TextIO.output(TextIO.stdErr, concat("CFG Warning: " :: msg @ ["\n"]))

    fun vl2s [] = "[]"
      | vl2s [x] = concat[V.toString x, ":", TyU.toString(V.typeOf x)]
      | vl2s (x::xs) = let
	  fun f (x, l) = "," :: V.toString x ::  ":" ::  TyU.toString(V.typeOf x) :: l
	  in
	    String.concat("[" :: V.toString x ::  ":" ::  TyU.toString(V.typeOf x)
	      :: List.foldr f ["]"] xs)
	  end

    fun tyl2s tys = TyU.toString(Ty.T_Tuple(false, tys))

    fun bindVar (env, x) = VSet.add(env, x)

    fun bindVars (env, xs) = VSet.addList(env, xs)

    fun check (m as CFG.MODULE{name, externs, code}) = let
	  val anyErrors = ref false
	(* construct a set of the bound labels in the module *)
	  val lSet = List.foldl
		(fn (f as CFG.FUNC{lab, ...}, lset) => LSet.add(lset, lab))
		  LSet.empty code
          val lMap = List.foldl
                (fn (f as CFG.FUNC{lab, entry, ...}, lmap) => LMap.insert(lmap,lab,entry))
                  LMap.empty code
	  fun chk (CFG.FUNC{lab, entry, body, exit}) = let
                fun err msg = (
		      anyErrors := true;
		      error (msg @ [" in ", Atom.toString name, ".", L.toString lab]))
                fun warn msg = warning (msg @ [" in ", Atom.toString name, ".", L.toString lab])
		fun chkVar (env, x) = if VSet.member(env, x)
		      then ()
		      else err[
			  "unbound variable ", V.toString x]
		fun chkVars (env, xs) = List.app (fn x => chkVar(env, x)) xs
		fun chkLabel l = (case (L.kindOf l, LSet.member(lSet, l))
		       of (CFG.LK_None, _) => err["label ", L.toString l, "has no kind"]
			| (CFG.LK_Extern _, false) => ()
			| (CFG.LK_Extern _, true) => err["extern local label ", L.toString l]
			| (CFG.LK_Local _, true) => ()
			| (CFG.LK_Local _, false) => err["reference to unbound label ", L.toString l]
		      (* end case *))
	      (* check the entry against the declared type of the label;  The declared type is allowed to
	       * be more specific.
	       *)
		fun chkEntry (CFG.StdFunc{clos, args, ret, exh}) = (
                      (case L.typeOf lab of
                          Ty.T_StdFun{clos = closTy, args = argTys, ret = retTy, exh = exhTy} => 
                             ((* FIXME: closure types not set by assignLabels *)
                              (*
                              if TyU.equal (V.typeOf clos, closTy)
                                 then ()
                              else err["variable ", V.toString clos, ":", TyU.toString (V.typeOf clos),
                                       " does not match ",
                                       "closure type ", TyU.toString closTy];
                              *)
                              if ListPair.allEq TyU.match (argTys, List.map V.typeOf args)
                        	then ()
                                else err[
				    "parameters ", vl2s args, " do not match ",
                                       "argument types ", tyl2s argTys
				  ];
                              if TyU.match (retTy, V.typeOf ret)
                                 then ()
                              else err["parameter ", V.toString ret, ":", TyU.toString (V.typeOf ret),
                                       " does not match ",
                                       "return type ", TyU.toString retTy];
                              if TyU.equal (exhTy, V.typeOf exh)
                                 then ()
                              else err["parameter ", V.toString exh, ":", TyU.toString (V.typeOf exh),
                                       " does not match ",
                                       "exh type ", TyU.toString exhTy])
                        | _ => err["label ", L.toString lab, ":", TyU.toString (L.typeOf lab),
                                   " is not stdfun"]);
		      bindVars(VSet.empty, clos::ret::exh::args))
		  | chkEntry (CFG.StdCont{clos, args}) = (
                      case L.typeOf lab
		       of Ty.T_StdCont{clos = closTy, args = argTys} => (
                           (* FIXME: closure types not set by assignLabels *)
                            (*
                            if TyU.equal (V.typeOf clos, closTy)
                               then ()
                            else err["variable ", V.toString clos, ":", TyU.toString (V.typeOf clos),
                                     " does not match ",
                                     "closure type ", TyU.toString closTy];
                            *)
                            if ListPair.allEq TyU.match (argTys, List.map V.typeOf args)
                              then ()
                              else err[
				  "parameters ", vl2s args, " do not match ",
                                     "argument types ", tyl2s argTys
				])
			| _ => err["label ", L.toString lab, ":", TyU.toString (L.typeOf lab),
                                   " is not stdcont"]
		      (* end case *);
		      bindVars(VSet.empty, clos::args))
		  | chkEntry (CFG.KnownFunc args) = (
                      case L.typeOf lab of
                          Ty.T_KnownFunc argTys => 
                             ((ListPair.appEq (fn (arg, argTy) =>
                                               if TyU.match (argTy, V.typeOf arg)
                                                  orelse TyU.match (argTy, Ty.T_Any)
                                                  then ()
                                               else err["parameter ", V.toString arg, ":", TyU.toString (V.typeOf arg),
                                                        " does not match ",
                                                        "argument type ", TyU.toString argTy])
                                              (args, argTys))
                              handle ListPair.UnequalLengths =>
                                 err["parameter (", String.concatWith "," (List.map V.toString args),
                                     ") do not match (", 
                                     String.concatWith "," (List.map TyU.toString argTys),
                                     ")"])
                        | _ => err["label ", L.toString lab, ":", TyU.toString (L.typeOf lab),
                                   " is not code"]
		      (* end case *);
		      bindVars(VSet.empty, args))
		  | chkEntry (CFG.Block args) = (
                      (case L.typeOf lab of
                          Ty.T_Block argTys => 
                             ((ListPair.appEq (fn (arg, argTy) =>
                                               if TyU.match (argTy, V.typeOf arg)
                                                  then ()
                                               else err["parameter ", V.toString arg, ":", TyU.toString (V.typeOf arg),
                                                        " does not match ",
                                                        "argument type ", TyU.toString argTy])
                                              (args, argTys))
                              handle ListPair.UnequalLengths =>
                                 err["parameters (", String.concatWith "," (List.map V.toString args),
                                     ") do not match (", 
                                     String.concatWith "," (List.map TyU.toString argTys),
                                     ")"])
                        | _ => err["label ", L.toString lab, ":", TyU.toString (L.typeOf lab),
                                   " is not code"]);
		      bindVars(VSet.empty, args))
		fun chkExp (e, env) = (case e
		       of CFG.E_Var(lhs, rhs) => let
			    fun chk (x, y) = if TyU.equal (V.typeOf x, V.typeOf y) then ()
                                    else err[
					"variable ", V.toString x, ":", TyU.toString (V.typeOf x),
                                        " does not match ",
                                        "variable ", V.toString y, ":", TyU.toString (V.typeOf y)
				      ]
			    in
			      chkVars (env, rhs);
                              (ListPair.appEq chk (lhs, rhs))
                                 handle ListPair.UnequalLengths => 
                                          err["variables (", String.concatWith "," (List.map V.toString lhs),
                                              ") do not match ", 
                                              "variables (", String.concatWith "," (List.map V.toString lhs),
                                              ")"];
			      bindVars (env, lhs)
                            end
			| CFG.E_Const(x, lit) => (
			    case (V.typeOf x, lit)
			     of (Ty.T_Enum wt, Lit.Enum w) => if Word.<= (w, wt) 
                        	  then ()
                                  else err[
				      "variable ", V.toString x, ":", TyU.toString (V.typeOf x),
                                      " is not ", TyU.toString (Ty.T_Enum wt)
				    ]
			      | _ => ()
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_Cast(x, ty, y) => (
			    chkVar (env, y);
                            if TyU.validCast (V.typeOf y, ty)
			      then ()
			      else err[
				  "variable ", V.toString y, ":", TyU.toString (V.typeOf y),
                                  " cannot be cast to ", "type ", TyU.toString ty
				];
                            if TyU.equal (V.typeOf x, ty)
			      then ()
			      else err[
				  "variable ", V.toString x, ":", TyU.toString (V.typeOf x),
				  " does not match type ", TyU.toString ty
				];
			    bindVar (env, x))
			| CFG.E_Label(x, lab) => (
			    chkLabel lab;
                            case L.kindOf lab
			     of CFG.LK_None => err["label ", L.toString lab, " has kind None"]
			      | CFG.LK_Extern _ => ()
			      | CFG.LK_Local _ => if TyU.equal (V.typeOf x, L.typeOf lab)
				  then ()
                                  else err[
				      "variable ", V.toString x, ":", TyU.toString (V.typeOf x),
				      " does not match label ",
				      L.toString lab, ":", TyU.toString (L.typeOf lab)
				    ]
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_Select(x, i, y) => let
			    val ty = TyU.select(V.typeOf y, i)
				  handle Fail msg => (
				    err["E_Select(", V.toString x, ", ", Int.toString i, ", ",
					V.toString y, ":", TyU.toString(V.typeOf y), ")"
				      ];
				    Ty.T_Any)
			    in
			      chkVar (env, y);
(* FIXME: Selecting from a known closure into an T_Any environment pointer fails *)
(*
                              if TyU.equal (V.typeOf x, ty)
                                 then ()
                              else err["variable ", V.toString x, ":", TyU.toString (V.typeOf x),
                                       " does not match ",
                                       "type ", TyU.toString ty];
*)
                              bindVar (env, x)
			    end
			| CFG.E_Update(i, y, z) => let
			    val ty = TyU.select(V.typeOf y, i)
				  handle Fail msg => (
				    err["E_Update(", Int.toString i, ", ",
					V.toString y, ":", TyU.toString(V.typeOf y), ", ",
					V.toString z, ")"
				      ];
				    Ty.T_Any)
			    in
			      chkVar (env, y);
(* FIXME: check that the tuple is mutable and that z has the right type *)
			      env
			    end
			| CFG.E_AddrOf(x, i, y) => let
			    val ty = TyU.select(V.typeOf y, i)
				  handle Fail msg => (
				    err["E_AddrOf(", V.toString x, ", ", Int.toString i, ", ",
					V.toString y, ":", TyU.toString(V.typeOf y), ")"
				      ];
				    Ty.T_Any)
			    in
			      chkVar (env, y);
			      case V.typeOf x
			       of Ty.T_Addr ty' => if TyU.equal(ty, ty')
				    then ()
				    else err[
					"type mismatch in E_AddrOf: lhs = ", TyU.toString ty',
					", rhs = ", TyU.toString ty, "\n"
				      ]
				| ty' => err[
					"type error in E_AddrOf: lhs = ", TyU.toString ty', "\n"
				      ]
			      (* end case *);
                              bindVar (env, x)
			    end
			| CFG.E_Alloc(x, ys) => (
			    chkVars (env, ys);
                            case V.typeOf x
                             of Ty.T_Tuple tys => ()
                              | Ty.T_OpenTuple tys => ()
			      | Ty.T_Any => ()
                              | _ => err["variable ", V.toString x, ":", TyU.toString (V.typeOf x),
                                         " does not match allocation"]
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_GAlloc(x, ys) => (
			    chkVars (env, ys);
                            case V.typeOf x
                             of Ty.T_Tuple tys => ()
                              | Ty.T_OpenTuple tys => ()
			      | Ty.T_Any => ()
                              | _ => err["variable ", V.toString x, ":", TyU.toString (V.typeOf x),
                                         " does not match allocation"]
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_Promote(x, y) => (
			    chkVar (env, y);
(* FIXME: we should check that x and y have the same type *)
			    bindVar (env, x))
			| CFG.E_Prim(x, p) => (
			    chkVars (env, PrimUtil.varsOf p);
			    bindVar (env, x))
			| CFG.E_CCall(lhs, f, args) => (
			    chkVars (env, f::args);
			    bindVars (env, lhs))
			| CFG.E_HostVProc vp => bindVar (env, vp)
			| CFG.E_VPLoad(x, _, vp) => (
			    chkVar (env, vp);
			    bindVar (env, x))
			| CFG.E_VPStore(_, vp, x) => (
			    chkVar (env, vp);
			    chkVar (env, x);
			    env)
		      (* end case *))
		fun chkExit (env, xfer) = (case xfer
		       of CFG.StdApply{f, clos, args, ret, exh} => (
			    chkVars (env, f :: clos :: ret :: exh :: args);
                            case V.typeOf f
			     of Ty.T_StdFun{clos = closTy, args = argTys, ret = retTy, exh = exhTy} => (
                                  if TyU.match (V.typeOf clos, closTy)
                                     then ()
                                  else warn["variable ", V.toString clos, ":", TyU.toString (V.typeOf clos),
                                           " does not match ",
                                           "closure type ", TyU.toString closTy];
                        	  if ListPair.allEq TyU.match (List.map V.typeOf args, argTys)
                                    then ()
                                    else err[
					"arguments ", vl2s args, " do not match ",
                                	   "argument types ", tyl2s argTys
				      ];
                                  if TyU.match (V.typeOf ret, retTy)
                                     then ()
                                  else err["variable ", V.toString ret, ":", TyU.toString (V.typeOf ret),
                                           " does not match ",
                                           "return type ", TyU.toString retTy];
                                  if TyU.match (V.typeOf exh, exhTy)
                                     then ()
                                  else err["variable ", V.toString exh, ":", TyU.toString (V.typeOf exh),
                                           " does not match ",
                                           "exh type ", TyU.toString exhTy])
                              | _ => err["variable ", V.toString f, ":", TyU.toString (V.typeOf f),
                                         " is not stdfun"]
			    (* end case *))
			| CFG.StdThrow{k, clos, args} => (
			    chkVars (env, k :: clos :: args);
                            case V.typeOf k
			     of Ty.T_StdCont{clos = closTy, args = argTys} => (
                                  if TyU.match (V.typeOf clos, closTy)
                                     then ()
                                  else warn["variable ", V.toString clos, ":", TyU.toString (V.typeOf clos),
                                           " does not match ",
                                           "closure type ", TyU.toString closTy];
                        	if ListPair.allEq TyU.match (List.map V.typeOf args, argTys)
                        	  then ()
                        	  else err[
				      "arguments ", vl2s args, " do not match ",
                                	 "argument types ", tyl2s argTys
				    ])
                              | _ => err["variable ", V.toString k, ":", TyU.toString (V.typeOf k),
                                         " is not stdcont"]
			    (* end case *))
			| CFG.Apply{f, args} => (
			    chkVars (env, f::args);
                            (case V.typeOf f of
                                Ty.T_KnownFunc argTys => 
                                   ((ListPair.appEq (fn (arg, argTy) =>
                                                     if TyU.match (V.typeOf arg, argTy)
                                                        then ()
                                                     else err["variable ", V.toString arg, ":", TyU.toString (V.typeOf arg),
                                                              " does not match ",
                                                              "argument type ", TyU.toString argTy])
                                                     (args, argTys))
                                    handle ListPair.UnequalLengths =>
                                             err["variables (", String.concatWith "," (List.map V.toString args),
                                                 ") do not match ", 
                                                 "variables (", String.concatWith "," (List.map TyU.toString argTys),
                                                 ")"])
                              | _ => err["variable ", V.toString f, ":", TyU.toString (V.typeOf f),
                                         " is not code"]))
			| CFG.Goto jmp => chkJump (env, jmp)
			| CFG.If(x, j1, j2) => (
			    chkVar (env, x);
                            if TyU.equal (V.typeOf x, Ty.boolTy)
                              then ()
                              else err["variable ", V.toString x, ":", TyU.toString (V.typeOf x), 
                                     " is not bool"];
			    chkJump (env, j1);
			    chkJump (env, j2))
			| CFG.Switch(x, cases, dflt) => (
			    chkVar (env, x);
			    case V.typeOf x
			     of Ty.T_Enum wt => let
				  fun chkCase (tag, jmp) = (
					if (tag <= wt)
                                	  then ()
                                	  else err[
					      "case ", Word.toString tag, " is out of range for ",
                                	      V.toString x, ":", TyU.toString (V.typeOf x)
					    ];
					chkJump(env, jmp))
				  in
				    List.app chkCase cases;
				    Option.app (fn j => chkJump(env, j)) dflt
				  end
                              | Ty.T_Raw rt => let
                                  fun chkCase (tag, jmp) = 
                                        chkJump(env, jmp)
                                  fun chk () = (
                                        List.app chkCase cases; 
                                        Option.app (fn j => chkJump(env, j)) dflt)
                                  fun bad () =
                                        err["variable ", V.toString x, ":", TyU.toString (V.typeOf x), 
                                            " is not valid argument for switch"]
                                  in
                                    case rt
                                     of RawTypes.T_Byte => chk ()
                                      | RawTypes.T_Short => chk ()
                                      | RawTypes.T_Int => chk ()
                                      | RawTypes.T_Long => chk ()
                                      | RawTypes.T_Float => bad ()
                                      | RawTypes.T_Double => bad ()
                                      | RawTypes.T_Vec128 => bad ()
                                  end
			      | _ => err["variable ", V.toString x, ":", TyU.toString (V.typeOf x), 
                                               " is not valid argument for switch"]
			    (* end case *))
			| CFG.HeapCheck{hck, szb, nogc = (lab, args)} => (
                            chkLabel lab;
                            chkVars (env, args);
                            case L.typeOf lab
			     of Ty.T_StdFun _ => err["noGC target is standard fun"]
                              | Ty.T_StdCont _ => err["noGC target is standard cont"]
                              | Ty.T_Block argTys => 
                                  ((ListPair.appEq (fn (arg, argTy) =>
                                                    if TyU.equal (V.typeOf arg, argTy)
                                                       then ()
                                                    else err["variable ", V.toString arg, ":", TyU.toString (V.typeOf arg),
                                                             " does not match ",
                                                             "argument type ", TyU.toString argTy])
                                                   (args, argTys))
                                   handle ListPair.UnequalLengths =>
                                      err["variables (", String.concatWith "," (List.map V.toString args),
                                          ") do not match ", 
                                          "variables (", String.concatWith "," (List.map TyU.toString argTys),
                                          ")"])
                              | _ => err["label ", L.toString lab, ":", TyU.toString (L.typeOf lab),
                                         " is not heap-check target"]
			    (* end case *))
			| CFG.AllocCCall{lhs, f, args, ret = (lab, rArgs)} => let
			      val rArgs = lhs @ rArgs
			      val env = bindVars (env, lhs)
			      in (
                                 chkLabel lab;
				 chkVars (env, f::args);
				 chkVars (env, rArgs);
			        case (V.typeOf f, lhs)
				 of (CFGTy.T_CFun (CFunctions.CProto (CFunctions.VoidTy, _, _)), x :: _) => 
				       err["incorrect c function type for, ", V.toString f]
				  | _ => ()
			         (* end case *);
				 case L.typeOf lab
				  of Ty.T_StdFun _ => err["ret target is standard fun"]
				   | Ty.T_StdCont _ => err["ret target is standard cont"]
				   | Ty.T_Block argTys => 
                                     ((ListPair.appEq (fn (arg, argTy) =>
							  if TyU.equal (V.typeOf arg, argTy)
							  then ()
							  else err["variable ", V.toString arg, ":", TyU.toString (V.typeOf arg),
								   " does not match ",
								"argument type ", TyU.toString argTy])
                                                      (rArgs, argTys))
                                      handle ListPair.UnequalLengths =>
					     err["variables (", String.concatWith "," (List.map V.toString rArgs),
						 ") do not match ", 
						 "variables (", String.concatWith "," (List.map TyU.toString argTys),
						 ")"])
				   | _ => err["label ", L.toString lab, ":", TyU.toString (L.typeOf lab),
                                              " is not heap-check target"]
 			         (* end case *))
			      end
 		         (* end case *))
		and chkJump (env, (lab, args)) = (
		      chkLabel lab;
		      chkVars (env, args);
                      case LMap.find (lMap, lab) of
                         SOME (CFG.Block argTys) => 
                           ((ListPair.appEq (fn (arg, argTy) =>
                                             if TyU.equal (V.typeOf arg, argTy)
                                                then ()
                                             else err["variable ", V.toString arg, ":", TyU.toString (V.typeOf arg),
                                                      " does not match ",
                                                      "argument type ", TyU.toString argTy])
                                            (args, List.map V.typeOf argTys))
                            handle ListPair.UnequalLengths =>
                               err["variables (", String.concatWith "," (List.map V.toString args),
                                   ") do not match ", 
                                   "variables (", String.concatWith "," (List.map (TyU.toString o V.typeOf) argTys),
                                   ")"])
                        | _ => err["label ", L.toString lab, " is not block"])
		val env = chkEntry entry
		val env = List.foldl chkExp env body
		in
		  chkExit (env, exit)
		end (* chk *)
	  in
	    List.app chk code;
	    if !anyErrors then raise Fail "broken CFG" else ()
	  end (* check *)
*)

    val check =
       BasicControl.mkTracePass
       {passName = "cfg-check",
        pass = check,
        verbose = 2}
  end

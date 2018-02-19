(* print-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintCFG : sig
    
    (* Max of int means the maximum number of characters to write out the type. 
       if the type's string length is longer, it is omitted. *)
    datatype verbosity = Full | Max of int | Silent
    
    type flags = {counts : bool, types : verbosity, preds : bool}

    val output : flags -> (TextIO.outstream * CFG.module) -> unit

    val print : CFG.module -> unit

    val printFunc : CFG.func -> unit

  end = struct
    
    datatype verbosity = Full | Max of int | Silent
    
    type flags = {counts : bool, types : verbosity, preds : bool}

    fun output (flags : flags) (outS, CFG.MODULE{name, externs, mantiExterns, code}) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl s = pr(String.concat s)
	  fun prIndent 0 = ()
	    | prIndent n = (pr "  "; prIndent(n-1))
	  fun indent i = prIndent i
	  fun prList toS [] = pr "()"
	    | prList toS [x] = pr(concat["(", toS x, ")"])
	    | prList toS l = let
		fun prL [] = ()
		  | prL [x] = pr(toS x)
		  | prL (x::r) = (pr(toS x); pr ","; prL r)
		in
		  pr "("; prL l; pr ")"
		end
    
      fun tyToStr prefix t = (case #types flags
               of Silent => ""
                | Full => prefix ^ CFGTyUtil.toString t
                | Max max => let
                  val typeStr = CFGTyUtil.toString t
                in
                  if String.size typeStr > max 
                  then ""
                  else prefix ^ typeStr
                end
              (* end case *))
        
	  fun varBindToString x = let
		val l = [tyToStr ":" (CFG.Var.typeOf x)]
		val l = if (#counts flags)
		      then "#" :: Int.toString(CFG.Var.useCount x) :: l
		      else l
		in
		  String.concat(CFG.Var.toString x :: l)
		end
	  fun varUseToString x = CFG.Var.toString x
	  fun labelBindToString lab = let
		val l = if (#counts flags)
		      then ["#", Int.toString(CFG.Label.useCount lab)]
		      else []
		in
		  String.concat("$" :: CFG.Label.toString lab :: l)
		end
	  fun labelUseToString lab = "$" ^ (CFG.Label.toString lab)
	  fun prParams []= pr "()"
	    | prParams [x] = pr(concat["(", varBindToString x, ")"])
	    | prParams params = let
		val params = List.map varBindToString params
		fun prl [x] = (pr x; pr "\n  )")
		  | prl (x::r) = (pr x; pr ",\n    "; prl r)
		in
		  pr "(\n    ";
		  prl params
		end

	  fun prPreds lab = 
	  	if (#preds flags) 
	  	then
	  		let
	  			fun runIt [] = ()
	  			  | runIt (src::nil) = pr (labelBindToString src)
	  			  | runIt (src::xs) = (pr (concat [labelBindToString src, ", "]) ; (runIt xs))
	  		in
	  			pr "\n";
	  			indent 2;
	  			(case CFG.maybeGetPreds lab
	  				of SOME preds =>
	  					(pr (concat ["(* ", ((Int.toString o List.length) preds), " preds:  "]) ;
			  			runIt preds ;
			  			pr " *)")
			  		 | NONE => pr "(* preds not set, unreachable? *) "
	  			(* end case *));
	  			pr "\n";
	  			indent 1
	  		end
	  	else
	  		()

	  fun prFunc (CFG.FUNC{lab, entry, start as CFG.BLK{args, exit, body=startbody, ...}, body}) = let
        val (kind, params, maybeRetTy) = (case (CFG.Label.kindOf lab, entry)
		       of (CFG.LK_Func{export=SOME _, ...}, CFG.StdFunc _) =>
			    ("export stdfun ", CFG.paramsOfConv(entry, args), NONE)
                
			| (CFG.LK_Func _, CFG.StdFunc _) =>
			    ("stdfun ", CFG.paramsOfConv(entry, args), NONE)        
                        
            | (CFG.LK_Func _, CFG.StdDirectFunc {ret=retTys,...}) =>
			    ("ds-stdfun ", CFG.paramsOfConv(entry, args), SOME retTys)
                
			| (CFG.LK_Func _, CFG.StdCont _) => 
                            ("cont ", CFG.paramsOfConv(entry, args), NONE)
                            
			| (CFG.LK_Func _, CFG.KnownFunc _) => 
                            ("kfun ", CFG.paramsOfConv(entry, args), NONE)
                            
            | (CFG.LK_Func _, CFG.KnownDirectFunc{ret=retTys,...}) =>
                            ("ds-kfun ", CFG.paramsOfConv(entry, args), SOME retTys)
                            
			| (CFG.LK_Block _, _) => ("block ", args, NONE)
			| _ => raise Fail "bogus function"
		      (* end case *))
        
        local
            val toTy = tyToStr ""
            fun pret [] = ()
              | pret [x] = pr (toTy x)
              | pret xs = let
                val xs = List.map toTy xs
                fun commas [x] = pr x
                  | commas (x::xs) = 
                    (pr x ; pr ", " ; commas xs)
              in
                commas xs
              end
        in
            fun prRetTy ts = (pr " -> "; pret ts)
        end
        
		in
		  indent 1;
		  pr kind;
		  prl [labelBindToString lab, " "]; 
                prParams params; Option.map prRetTy maybeRetTy; pr " =\n";
		  List.app (prExp 2) startbody;
		  prXfer (2, exit);
                  List.app prBlock body
		end
          and prBlock (b as CFG.BLK{lab,args,exit,body}) = (
              indent 1;
              pr "block ";
              prl [labelBindToString lab, " "]; prPreds lab; prParams args; pr " =\n";
              List.app (prExp 2) body;
              prXfer (2, exit))
	  and prExp i e = (
		indent i;
		case CFG.lhsOfExp e
		 of [] => pr "do "
		  | [x] => prl["let ", varBindToString x, " = "]
		  | xs => (pr "let "; prList varBindToString xs; pr " = ")
		(* end case *);
		case e
		 of (CFG.E_Var(_, ys)) => prList varUseToString ys
		  | (CFG.E_Const(_, lit, ty)) => prl[
                        Literal.toString lit, ":", CFGTyUtil.toString ty
                      ]
		  | (CFG.E_Cast(_, ty, y)) => prl["(", CFGTyUtil.toString ty, ")", varUseToString y]
		  | (CFG.E_Label(_, lab)) => pr(labelUseToString lab)
		  | (CFG.E_Select(_, i, x)) =>
		      prl ["#", Int.toString i, " ", varUseToString x]
		  | (CFG.E_Update(i, x, z)) => prl [
			"#", Int.toString i, " ", varUseToString x, " := ", varUseToString z
		      ]
		  | (CFG.E_AddrOf(_, i, x)) =>
		      prl ["&", Int.toString i, " ", varUseToString x]
		  | (CFG.E_Alloc(_, ty, args)) => let
		      val mut = (case ty of CFGTy.T_Tuple(true, _) => "alloc !" | _ => "alloc ")
		      in
			pr mut; prList varUseToString args
		      end
		  | (CFG.E_GAlloc(_, ty, args)) => let
		      val mut = (case ty of CFGTy.T_Tuple(true, _) => "galloc !" | _ => "galloc ")
		      in
			pr mut; prList varUseToString args
		      end
		  | (CFG.E_Promote(_, y)) => prl["promote(", varUseToString y, ")"]
		  | (CFG.E_Prim0 p) => pr (PrimUtil.fmt varUseToString p)
		  | (CFG.E_Prim(_, p)) => pr (PrimUtil.fmt varUseToString p)
		  | (CFG.E_CCall(_, f, args)) => (
		      prl ["ccall ", varUseToString f, " "];
		      prList varUseToString args)
		  | (CFG.E_HostVProc _) => pr "host_vproc()"
		  | (CFG.E_VPLoad(_, offset, vp)) => prl [
			"vpload(", varUseToString vp, "+", IntInf.toString offset, ")"
		      ]
		  | (CFG.E_VPStore(offset, vp, x)) => prl [
			"vpstore(", varUseToString vp, "+", IntInf.toString offset, ",",
			varUseToString x, ")"
		      ]
		  | (CFG.E_VPAddr(_, offset, vp)) => prl [
			"vpaddr(", varUseToString vp, "+", IntInf.toString offset, ")"
		      ]
		(* end case *);
		pr "\n")
	  and prXfer (i, xfer) = (
		indent i;
		case xfer
		 of CFG.StdApply{f, clos, args, ret, exh} =>
		      prApply("stdApply", f, clos :: args @ [ret, exh])
		  | CFG.StdThrow{k, clos, args} =>
		      prApply("stdThrow", k, clos :: args)
		  | CFG.Apply{f, clos, args} => prApply("apply", f, clos :: args)
		  | CFG.Goto jmp => prJump("goto", jmp)
		  | CFG.HeapCheck{hck, szb, nogc} => let 
                      val check = (case hck
                          of CFG.HCK_Local => "check"
                           | CFG.HCK_Global => "checkGlobal"
                          (* end case *))
                      in (
		        pr check; pr " (avail-mem < "; pr(Word.fmt StringCvt.DEC szb); pr ")\n";
		        indent (i+1); pr "then GC()\n";
		        indent (i+1); prJump("else", nogc))
                      end
		  | CFG.HeapCheckN{hck, n, szb, nogc} => let 
                      val check = (case hck
                          of CFG.HCK_Local => "check"
                           | CFG.HCK_Global => "checkGlobal"
                          (* end case *))
                      in (
		        pr check; pr " (avail-mem < n)\n";
		        indent (i+1); pr "then GC()\n";
		        indent (i+1); prJump("else", nogc))
                      end
		  | CFG.AllocCCall {lhs, f, args, ret=(l,rArgs)} => (
              prCall("ccall-alloc", lhs, f, args);
              indent (i+1); 
                  prJump("", (l,lhs@rArgs))  (* the post call jump *)
            )
		      
          | CFG.Call {f, clos, args, next=SOME(lhs, (afterL, liveAfter))} => (
              prCall("call", lhs, f, clos::args);
              indent (i+1); 
                prJump("next", (afterL, liveAfter))
            )
          
          | CFG.Call {f, clos, args, next=NONE} =>
              prCall("tailcall", nil, f, clos::args)
          
          | CFG.Return {args, name} => prReturn("return", args, name)
           
		  | CFG.If(cond, j1, j2) => (
		      prl ["if ", CondUtil.fmt varUseToString cond, "\n"];
		      indent (i+1); prJump("then", j1);
		      indent (i+1); prJump("else", j2))
		  | CFG.Switch(x, cases, dflt) => let
		      fun prCase (c, jmp) = (
			    indent (i+1);
			    prl ["case 0x", Word.toString c, ": "];
			    prJump("", jmp))
		      in
			prl ["switch ", varUseToString x, "\n"];
			List.app prCase cases;
			case dflt
			 of NONE => ()
			  | SOME jmp => (indent(i+1); prJump("default: ", jmp))
			(* end case *)
		      end
		(* end case *))
	  and prApply (prefix, x, args) = (
		prl [prefix, " ", varUseToString x];
		prList varUseToString args;
		pr "\n")
	  and prJump (prefix, (lab, args)) = (
		prl [prefix, " ", labelUseToString lab];
		prList varUseToString args;
		pr "\n")
      
      and prCall (prefix, nil, f, args) = (
            prl [prefix, " "];
            prl [varUseToString f, " "];
            prList varUseToString args;
            pr "\n"
        )
        | prCall (prefix, lhs, f, args) = (
            prl [prefix, " "];
            prList varBindToString lhs;
            prl [" = ", varUseToString f, " "];
            prList varUseToString args;
            pr "\n"
        )
        
      and prReturn (prefix, args, name) = (
          prl [prefix, " "];
          prList varUseToString args;
          prl [" from ", labelUseToString name];
          pr "\n"
      )
      
        
	  fun prExtern cf = prl["  ", CFunctions.cfunToString cf, "\n"]
      
      fun prManiExtern lab = let
            val (CFG.LK_Extern name) = CFG.Label.kindOf lab
            val ty = CFG.Label.typeOf lab 
      in
        prl ["  ", name, " : ", CFGTyUtil.toString ty, "\n"]
      end
      
(*
	  fun prExtern (CFunctions.CFun{var, ...}) = prl[
		  "  extern ", labelUseToString var, " : ",
		  CFGTy.toString(CFG.Label.typeOf var), "\n"
		]
*)
	  in
	    prl ["module ", Atom.toString name, " {\n"];
	    List.app prExtern externs;
	    List.app prManiExtern mantiExterns;
	    List.app prFunc code;
	    pr "}\n"
	  end

    fun print m = output {counts=true, types=Silent, preds=false} (TextIO.stdOut, m)

    fun printFunc f = let
          val m = CFG.MODULE {name = Atom.atom "ad-hoc",
			      externs = [],
			      mantiExterns = [],
			      code = [f]}
          in
            print m
          end

  end

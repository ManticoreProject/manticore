structure TreeShake =
  struct

    structure PT = ProgramParseTree.PML2
    structure BPT = PT.BOMParseTree
    structure Var = ProgramParseTree.Var

    fun concatMap f ls = List.concat(List.map f ls)


  (* we use a single variable to represent top-level wild-card patterns *)
    val wildVar = Var.new("wild", ())

    fun debug() = Controls.get BasicControl.treeShakeDebug
	
    fun varsOfPat pat = (
	  case pat
	   of PT.MarkPat {tree, ...} => varsOfPat tree
	    | PT.BinaryPat (p1, v, p2) => v :: varsOfPat p1 @ varsOfPat p2
	    | PT.ConPat (v, p) => v :: varsOfPat p
	    | PT.TuplePat [] => [wildVar]
	    | PT.TuplePat ps => concatMap varsOfPat ps
	    | PT.ConstraintPat (p, t) => varsOfPat p
	    | PT.IdPat v => [v]
	    | PT.WildPat => [wildVar]
	    | _ => []
          (* end case *))

    fun bindsOfFunct (PT.MarkFunct {tree, ...}, m) = bindsOfFunct (tree, m)
      | bindsOfFunct (PT.Funct clauses, m) = List.map (fn (f, _, _, _) => (m,f)) clauses

  (* get the bound variables of a value declaration *)
    fun bindsOfValDecl (vd, m) = (
	  case vd
	   of PT.MarkVDecl {tree, ...} => bindsOfValDecl (tree, m)
	    | ( PT.ValVDecl (p, _) ) => List.map (fn v => (m,v)) (varsOfPat p)
	    | ( PT.PValVDecl  (p, _)) => List.map (fn v => (m,v)) (varsOfPat p)
	    | ( PT.PrimVDecl (p, _) ) => List.map (fn v => (m,v)) (varsOfPat p)
	    | PT.FunVDecl funs => List.concat (List.map (fn f => bindsOfFunct (f,m)) funs)
          (* end case *))

    fun bindsOfPrimCode (defn, m) = (
	  case defn
	   of BPT.D_Mark {tree, ...} => bindsOfPrimCode (tree, m)
	    | BPT.D_Define (_, f, _, _, _, _) => [(m,f)]
	    | BPT.D_ImportML (_, hlopid, _) => [(m,hlopid)]
	    | _ => []
          (* end case *))

    fun bindsOfTyDecl (tyDecl, m) = (
          case tyDecl
	   of PT.MarkTyDecl {tree, ...} => bindsOfTyDecl (tree, m)
	    | PT.TypeTyDecl (_, ty, _) => [(m,ty)]
	    | PT.DataTyDecl dts => List.map (fn (_,y,_) => (m,y)) dts
	    | PT.DataTyReplDecl (ty, _) => [(m,ty)]
	    | PT.AbsTyDecl (_, ty) => [(m,ty)]
	    | PT.PrimTyDecl (_, ty, _) => [(m,ty)]
          (* end case *))

(*    fun bindsOfTyDecls (decls, m) = List.concat(List.map (fn (d) => bindsOfTyDecl (d, m)) decls)

    fun bindsOfSpec (spec, m) = (
        case spec
         of PT.MarkSpec {tree,...} => bindsOfSpec (tree,m)
          | PT.IncludeSpec (sign) => bindsOfSign (sign, m)
          | PT.ModuleSpec (m', sign) =>  bindsOfSign (sign, if (m="") then (Var.nameOf m') else concat[m, ".", Var.nameOf m'])
          | PT.TypeSpec (tydec) => bindsOfTyDecl (tydec, m)
          | PT.ConstSpec (_, _) => []
          | PT.ValSpec (v, _, _) => [(m,v)])

    and bindsOfSpecs (specs, m) = List.concat(List.map (fn (d) => bindsOfSpec (d, m)) specs)

    and bindsOfSign (sign, m) = (
        case sign
         of PT.MarkSig {tree,...} => bindsOfSign (tree, m)
          | PT.NameSig (sigid, tydecls) =>  (m,sigid)::(bindsOfTyDecls (tydecls, if (m="") then (Var.nameOf sigid) else concat[m, ".", Var.nameOf sigid]))
          | PT.ExpSig (specs) => bindsOfSpecs (specs, m))
*)
    fun bindsOfModule (module, m) = (
	  case module
	   of PT.MarkMod {tree, ...} => bindsOfModule (tree, m)
	    | PT.DeclsMod decls => bindsOfDecls (decls, m)
	    | PT.ApplyMod (_, modules) => concatMap (fn module => bindsOfModule(module, m)) modules
	    | _ => []
          (* end case *))

    and bindsOfDecl (decl, m) = (
	  case decl
	   of PT.MarkDecl {tree, ...} => bindsOfDecl (tree, m)
	    | PT.ModuleDecl (name, _, module) => bindsOfModule (module, if (m="") then (Var.nameOf name) else concat[m, ".", Var.nameOf name])
	    | PT.TyDecl tyDecl => bindsOfTyDecl (tyDecl, m)
	    | PT.ExnDecl (exn, _) => [(m,exn)]
	    | PT.ValueDecl vd => bindsOfValDecl (vd, m)
	    | PT.LocalDecl (ds1, ds2) => bindsOfDecls (ds1, m) @ bindsOfDecls (ds2, m)
(*            | PT.SignDecl (sig_id, sign) => (m,sig_id)::(bindsOfSign (sign,m)) *)
	    | PT.PrimCodeDecl defns => concatMap (fn d => bindsOfPrimCode(d, m)) defns
	    | _ => []
          (* end case *))

  (* bound variables of a declaration *)
    and bindsOfDecls (decls, m) = List.concat(List.map (fn (d) => bindsOfDecl (d, m)) decls)

  (* property that contains the outgoing edges of a varable *)
    val {
           getFn=getEdges : Var.var -> Var.var list, 
	   setFn=setEdges' : (Var.var * Var.var list) -> unit, ...
        } = 
	   Var.newProp (fn _ => [])

  (* property that determines whether a function is dead code *)
    local
    val {
           getFn : Var.var -> bool, 
	   setFn : (Var.var * bool) -> unit, ...
        } = 
	   Var.newProp (fn _ => false)
    in
    fun setDead v = ((*print(Var.toString v^"=dead\n");*)setFn(v, true))
    val isDead = getFn
    fun setEdges (var, edges) = setEdges'(var, getEdges var@edges)
    end


  (* record the outgoing edges of a value declaration *)
    fun setEdgesOfValDecl vd = let
	  val es = Var.Set.listItems(UsedVars.usedOfValDecl vd)
	  val _ = if debug() then print ("vd:"^String.concatWith "\n" (List.map (fn (m,v) => m ^ (Var.toString v)) (bindsOfValDecl (vd,"")))^":\n") else ()
	  val _ = if debug() then print (String.concatWith "\n" (List.map Var.toString es)^"\n") else ()

          in
	     List.app (fn (_,v) => setEdges (v, es)) (bindsOfValDecl (vd,""))
          end

    fun setEdgesOfPrimCode defns = List.app setEdgesOfDefn defns

    and setEdgesOfDefn defn = (
	  case defn
	   of BPT.D_Mark {tree, ...} => setEdgesOfDefn tree
	    | BPT.D_Define (_, v, _, _, _, _) => setEdges(v, Var.Set.listItems (BOMUsedVars.usedOfDefn defn))
	    | BPT.D_ImportML (_, hlopid, pmlv) => setEdges(hlopid, [pmlv])
	    | _ => ()
          (* end case *))

    fun setEdgesOfModule module = (
	  case module
	   of PT.MarkMod {tree, ...} => setEdgesOfModule tree
	    | PT.DeclsMod decls => setEdgesOfDecls decls
	    | PT.ApplyMod (_, mods) => List.app setEdgesOfModule mods
	    | _ => ()
          (* end case *))

    and setEdgesOfDecl decl = (
	  case decl
	   of PT.MarkDecl {tree, ...} => setEdgesOfDecl tree
	    | PT.ValueDecl vd => setEdgesOfValDecl vd
	    | PT.LocalDecl (ds1, ds2) => (setEdgesOfDecls ds1; setEdgesOfDecls ds2)
	    | PT.PrimCodeDecl c => setEdgesOfPrimCode c
	    | PT.ModuleDecl (_, _, module) => setEdgesOfModule module
	    | _ => ()
          (* end case *))

    and setEdgesOfDecls decls = List.app setEdgesOfDecl decls

    fun isRoot (PT.MarkVDecl {tree, ...}) = isRoot tree
      | isRoot (PT.ValVDecl _) = true
      | isRoot (PT.PValVDecl _) = true
      | isRoot _ = false

    fun rootsOfModule module = (
	  case module
	   of PT.MarkMod {tree, ...} => rootsOfModule tree
	    | PT.DeclsMod decls => rootsOfDecls decls
	    | PT.ApplyMod (_, modules) => concatMap rootsOfModule modules
	    | _ => []
          (* end case *))

    and rootsOfDecl decl = (
	  case decl
	   of PT.MarkDecl {tree, ...} => rootsOfDecl tree
	    | PT.ModuleDecl (_, _, module) => rootsOfModule module
	    | PT.ValueDecl vd =>
	        if isRoot vd
		   then bindsOfValDecl (vd, "")
		else []
	    | PT.LocalDecl (ds1, ds2) => rootsOfDecls ds1 @ rootsOfDecls ds2
            | PT.PrimCodeDecl (code) => concatMap rootsOfPrimCode code
            | _ => []
          (* end case *))

    and rootsOfPrimCode defn = bindsOfPrimCode (defn, "")

    and rootsOfDecls decls = concatMap rootsOfDecl decls

    structure SCC = GraphSCCFn (Var.Set.Key)

    fun varsOfCC (SCC.SIMPLE v) = [v]
      | varsOfCC (SCC.RECURSIVE vs) = vs

    val flattenCCs = concatMap varsOfCC

  (* given module roots, return the set of used variables *)
    fun usedVars roots = let
          val vars = List.map (fn (_,v) => v) roots
	  val ccs = SCC.topOrder' {roots=vars, follow=getEdges}
          in
	     Var.Set.fromList(flattenCCs ccs)
	  end

    structure Key =
      struct
	type ord_key = string
	val compare = String.compare
      end
    structure StringSet = RedBlackSetFn (Key)

  (* mark function definitions for removal *)
    fun setDeadFuns ds = let
	  val _ = setEdgesOfDecls ds
          val extraRootStrings = List.foldr (fn (l, s) => StringSet.add (s, String.concatWith "." l)) StringSet.empty
                                            (DelayedBasis.allVars() @ DelayedBasis.allTyCons() @ DelayedBasis.allDataCons() @ DelayedBasis.allHLOps())
          val _ = if debug() then print (concat["Extra Root Strings :", Int.toString (StringSet.numItems extraRootStrings), "\n"]) else ()
          val _ = if debug() then StringSet.app (fn (v) => print (concat[v, "\n"])) extraRootStrings  else ()
	  val binds =  bindsOfDecls (ds, "")
          val _ = if debug() then print "Binds:\n" else ()
          val _ = if debug() then List.app (fn (m,v) => print (concat[m, ".", Var.toString v, "\n"])) binds else ()
          val extraRoots = List.filter (fn (m, v) => StringSet.member(extraRootStrings, if (m="") then Var.nameOf v else concat[m, ".", Var.nameOf v])) binds
          val _ = if debug() then print (concat["Extra Root Vars :", Int.toString (List.length extraRoots), "\n"]) else ()
          val _ = if debug() then List.app (fn (m, v) => print (concat[m, ".", Var.toString v, "\n"])) extraRoots  else ()
	  val live = usedVars (rootsOfDecls ds @ extraRoots)
          val _ = if debug() then print (concat["Live vars : ", Int.toString (Var.Set.numItems live), "\n"]) else ()
          val _ = if debug() then Var.Set.app (fn (v) => print (concat[Var.toString v, "\n"])) live else ()
	  fun isDead (m,v) = not(Var.Set.member(live, v))
          in
	     List.app (fn (_,v) => setDead v) (List.filter isDead binds)
	  end

    fun shakeDefn defn = (
	  case defn
	   of BPT.D_Mark {tree, span} =>
	        shakeDefn tree
	    | BPT.D_Define (_, hlop, _, _, _, _) =>
	        not(isDead hlop)
	    | _ => true
          (* end case *))

    fun shakeCode code = List.filter shakeDefn code	  

    fun isFunDead (PT.MarkFunct {tree, span}) = isFunDead tree
      | isFunDead (PT.Funct clauses) = List.all (fn (f, _, _, _) => isDead f) clauses

    fun shakeFunct (funct as PT.MarkFunct {tree, span}) = 
	  shakeFunct tree
      | shakeFunct (funct as PT.Funct _) =
	  not(isFunDead funct)

    and shakeFuncts functs = List.filter shakeFunct functs

    fun shakeValDecl valDecl = (
	  case valDecl
	   of PT.MarkVDecl {tree, span} => 
	        PT.MarkVDecl {tree=shakeValDecl tree, span=span}
	    | PT.FunVDecl funs => 
	        PT.FunVDecl(shakeFuncts funs)
	    | _ => valDecl
          (* end case *))

    fun shakeModule module = (
	  case module
	   of PT.MarkMod {tree, span} =>
	        PT.MarkMod {tree=shakeModule tree, span=span}
	    | PT.DeclsMod decls =>
	        PT.DeclsMod (shakeDecls' decls)
	    | PT.ApplyMod(m, modules) =>
	        PT.ApplyMod(m, shakeModules modules)
	    | _ => module
          (* end case *))

    and shakeModules modules = List.map shakeModule modules

  (* shake off a useless declaration *)
    and shakeDecl decl = (
	  case decl
	   of PT.MarkDecl {tree, span} => 
	        PT.MarkDecl{tree=shakeDecl tree, span=span}
	    | PT.ValueDecl valDecl => 
	        PT.ValueDecl(shakeValDecl valDecl)
	    | PT.LocalDecl (decls1, decls2) => 
	        PT.LocalDecl(shakeDecls' decls1, shakeDecls' decls2)
	    | PT.ModuleDecl (mb, sign, module) =>
	        PT.ModuleDecl(mb, sign, shakeModule module)
	    | PT.PrimCodeDecl code =>
	        PT.PrimCodeDecl (shakeCode code)
	    | _ => decl
          (* end case *))

    and shakeDecls' decls = List.map shakeDecl decls

    and shakeDecls {tree, span} = {tree=List.map shakeDecl tree, span=span}

    and shakeProgram program = List.map shakeDecls program
	        

  end

structure TreeShake =
  struct

    structure PT = ProgramParseTree.PML2
    structure BPT = PT.BOMParseTree
    structure Var = ProgramParseTree.Var

    fun concatMap f ls = List.concat(List.map f ls)

  (* property that contains the outgoing edges of a varable *)
    val {
           getFn=getEdges : Var.var -> Var.var list, 
	   setFn=setEdges : (Var.var * Var.var list) -> unit, ...
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
    end

    val wildVar = Var.new("wild", ())
	
    fun varsOfPat pat = (
	  case pat
	   of PT.MarkPat {tree, ...} => varsOfPat tree
	    | PT.BinaryPat (p1, v, p2) => v :: varsOfPat p1 @ varsOfPat p2
	    | PT.ConPat (v, p) => v :: varsOfPat p
	    | PT.TuplePat ps => concatMap varsOfPat ps
	    | PT.ConstraintPat (p, t) => varsOfPat p
	    | PT.IdPat v => [v]
	    | PT.WildPat => [wildVar]
	    | _ => []
          (* end case *))

    fun bindsOfFunct (PT.MarkFunct {tree, ...}) = bindsOfFunct tree
      | bindsOfFunct (PT.Funct (v, _, _)) = v

  (* get the bound variables of a value declaration *)
    fun bindsOfValDecl vd = (
	  case vd
	   of PT.MarkVDecl {tree, ...} => bindsOfValDecl tree
	    | ( PT.ValVDecl (p, _) |
		PT.PValVDecl  (p, _) |
		PT.PrimVDecl (p, _) ) => varsOfPat p
	    | PT.FunVDecl funs => List.map bindsOfFunct funs
          (* end case *))

    fun bindsOfPrimCode defn = (
	  case defn
	   of BPT.D_Mark {tree, ...} => bindsOfPrimCode tree
	    | BPT.D_Define (_, f, _, _, _, _) => [f]
	    | _ => []
          (* end case *))          

    fun bindsOfModule module = (
	  case module
	   of PT.MarkMod {tree, ...} => bindsOfModule tree
	    | PT.DeclsMod decls => bindsOfDecls decls
	    | PT.ApplyMod (_, modules) => concatMap bindsOfModule modules
	    | _ => []
          (* end case *))

    and bindsOfDecl decl = (
	  case decl
	   of PT.MarkDecl {tree, ...} => bindsOfDecl tree
	    | PT.ModuleDecl (_, _, module) => bindsOfModule module
	    | PT.ValueDecl vd => bindsOfValDecl vd
	    | PT.LocalDecl (ds1, ds2) => bindsOfDecls ds1 @ bindsOfDecls ds2
	    | PT.PrimCodeDecl defns => concatMap bindsOfPrimCode defns
	    | _ => []
          (* end case *))

    and bindsOfDecls decls = List.concat(List.map bindsOfDecl decls)

  (* record the outgoing edges of a value declaration *)
    fun setEdgesOfValDecl vd = let
	  val es = Var.Set.listItems(UsedVars.usedOfValDecl vd)
(*	  val _ = print ("vd:"^String.concatWith "\n" (List.map Var.toString (bindsOfValDecl vd))^":\n")
	  val _ = print (String.concatWith "\n" (List.map Var.toString es)^"\n")
*)
          in
	     List.app (fn v => setEdges (v, es)) (bindsOfValDecl vd)
          end

    fun setEdgesOfPrimCode defns = List.app setEdgesOfDefn defns

    and setEdgesOfDefn defn = (
	  case defn
	   of BPT.D_Mark {tree, ...} => setEdgesOfDefn tree
	    | BPT.D_Define (_, v, _, _, _, _) => setEdges(v, Var.Set.listItems (BOMUsedVars.usedOfDefn defn))
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
		   then bindsOfValDecl vd
		else []
	    | PT.LocalDecl (ds1, ds2) => rootsOfDecls ds1 @ rootsOfDecls ds2
	    | _ => []
          (* end case *))

    and rootsOfDecls decls = concatMap rootsOfDecl decls

    structure SCC = GraphSCCFn (Var.Set.Key)

    fun varsOfCC (SCC.SIMPLE v) = [v]
      | varsOfCC (SCC.RECURSIVE vs) = vs

    val flattenCCs = concatMap varsOfCC

  (* given module roots, return the set of used variables *)
    fun usedVars roots = let
	  val ccs = SCC.topOrder' {roots=roots, follow=getEdges}
          in
	     Var.Set.fromList(flattenCCs ccs)
	  end

    fun setDeadFuns ds = let
	  val _ = setEdgesOfDecls ds
	  val live = usedVars (rootsOfDecls ds)
	  val binds =  bindsOfDecls ds
	  fun isDead v = not(Var.Set.member(live, v))
          in
	     List.app setDead (List.filter isDead binds)
	  end

  end

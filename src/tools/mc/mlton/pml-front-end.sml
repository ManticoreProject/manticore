(* pml-front-end.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module packages up the front-end components of the MLton compiler
 * to produce an SXML representation of the program.
 *)

structure PMLFrontEnd : PML_FRONT_END =
  struct

    structure Ctl = Control
    structure MVec = MLtonVector

  (*---------------------------------------------------*)
  (*              Intermediate Languages               *)
  (*---------------------------------------------------*)

    structure Symbol = Symbol ()
    structure Field = Field (structure Symbol = Symbol)
    structure Record = Record (
	val isSorted = false
	structure Field = Field)
    structure SortedRecord = Record (
	val isSorted = true
	structure Field = Field)
    structure Tyvar = Tyvar ()
    structure Ast = Ast (
	structure Record = Record
	structure SortedRecord = SortedRecord
	structure Symbol = Symbol
	structure Tyvar = Tyvar)
    local
      open Ast.Tycon
    in
    structure CharSize = CharSize
    structure IntSize = IntSize
    structure RealSize = RealSize
    structure WordSize = WordSize
    end (* local open Ast.Tycon *)
    structure Atoms = Atoms (
	structure CharSize = CharSize
	structure Field = Field
	structure IntSize = IntSize
	structure RealSize = RealSize
	structure Record = Record
	structure SortedRecord = SortedRecord
	structure Tyvar = Tyvar
	structure WordSize = WordSize)
    local
      open Atoms
    in
    structure Const = Const
    structure ConstType = Const.ConstType
    structure Ffi = Ffi
    structure WordX = WordX
    end
    structure TypeEnv = TypeEnv (Atoms)
    structure CoreML = CoreML (
	open Atoms
	structure Type =
	  struct
	    open TypeEnv.Type

	    val makeHom =
		 fn {con, var} => makeHom {con = con, expandOpaque = true, var = var}

	    fun layout t = layoutPrettyAux (t, {expandOpaque = true, localTyvarNames = false})
	   end)
    structure Xml = Xml (open Atoms)
    structure Sxml = Sxml (open Xml)


  (*---------------------------------------------------*)
  (*                  Compiler Passes                  *)
  (*---------------------------------------------------*)

    structure FrontEnd = FrontEnd (structure Ast = Ast)
    structure MLBFrontEnd = MLBFrontEnd (
	structure Ast = Ast
	structure FrontEnd = FrontEnd)
    structure DeadCode = DeadCode (structure CoreML = CoreML)
    structure Defunctorize = Defunctorize (
	structure CoreML = CoreML
	structure Xml = Xml)
    structure Elaborate = Elaborate (
	structure Ast = Ast
	structure CoreML = CoreML
	structure TypeEnv = TypeEnv)
    structure Env = Elaborate.Env
    structure LookupConstant = LookupConstant (
	structure Const = Const
	structure ConstType = ConstType
	structure Ffi = Ffi)
    structure Monomorphise = Monomorphise (
	structure Xml = Xml
	structure Sxml = Sxml)



(* ------------------------------------------------- *)   
(*                   Primitive Env                   *)
(* ------------------------------------------------- *)

    local
      structure Con = TypeEnv.Con
      structure Tycon = TypeEnv.Tycon
      structure Type = TypeEnv.Type
      structure Tyvar = TypeEnv.Tyvar

      val primitiveDatatypes = let
	    val boolTyc = {
		    tycon = Tycon.bool,
		    tyvars = MVec.new0 (),
		    cons = MVec.new2 (
		      {con = Con.falsee, arg = NONE},
		      {con = Con.truee, arg = NONE})
		  }
	    val listTyc = let
		  val a = Tyvar.newNoname {equality = false}
		  in {
		    tycon = Tycon.list,
		    tyvars = MVec.new1 a,
		    cons = MVec.new2 (
		      {con = Con.nill, arg = NONE},
		      {con = Con.cons,
		       arg = SOME(Type.tuple(MVec.new2(Type.var a, Type.list (Type.var a))))
		      })
		  } end
	    val refTyc = let
		  val a = Tyvar.newNoname {equality = false}
		  in {
		    tycon = Tycon.reff,
		    tyvars = MVec.new1 a,
		    cons = MVec.new1 {con = Con.reff, arg = SOME(Type.var a)}
		  } end
	    in
	      MVec.new3 (boolTyc, listTyc, refTyc)
	    end

      val primitiveExcons = [
	      CoreML.Con.bind, CoreML.Con.match, CoreML.Con.overflow
	    ]

      structure Con = struct
	  open Con

	  fun toAst c = Ast.Con.fromSymbol (Symbol.fromString (Con.toString c), Region.bogus)
	end

      structure Env = struct
	  open Env 
  
	  structure Tycon = struct
	      open Tycon
  
	      fun toAst c = Ast.Tycon.fromSymbol (Symbol.fromString (Tycon.toString c), Region.bogus)
	    end
	  structure Type = TypeEnv.Type
	  structure Scheme = TypeEnv.Scheme
  
	  fun addPrim (E: t): unit = let
		val _ =
		    MLtonList.foreach
		    (Tycon.prims, fn {kind, name, tycon, ...} =>
		     extendTycon
		     (E, Ast.Tycon.fromSymbol (Symbol.fromString name,
					       Region.bogus),
		      TypeStr.tycon (tycon, kind),
		      {forceUsed = false, isRebind = false}))
		val _ =
		    MVec.foreach
		    (primitiveDatatypes, fn {tyvars, tycon, cons} =>
		     let
			val cons =
			   Env.newCons
			   (E, MVec.map (cons, fn {con, ...} =>
					   {con = con, name = Con.toAst con}))
			   (MVec.map
			    (cons, fn {arg, ...} =>
			     let
				val resultType =
				   Type.con (tycon, MVec.map (tyvars, Type.var))
			     in
				Scheme.make
				{canGeneralize = true,
				 ty = (case arg of
					  NONE => resultType
					| SOME t => Type.arrow (t, resultType)),
				 tyvars = tyvars}
			     end))
		     in
			extendTycon
			(E, Tycon.toAst tycon,
			 TypeStr.data (tycon,
				       TypeStr.Kind.Arity (Vector.length tyvars),
				       cons),
			 {forceUsed = false, isRebind = false})
		     end)
		 val _ =
		    extendTycon (E,
				 Ast.Tycon.fromSymbol (Symbol.unit, Region.bogus),
				 TypeStr.def (Scheme.fromType Type.unit,
					      TypeStr.Kind.Arity 0),
				 {forceUsed = false, isRebind = false})
		 val scheme = Scheme.fromType Type.exn
		 val _ = MLtonList.foreach (primitiveExcons, fn c =>
				       extendExn (E, Con.toAst c, c, SOME scheme))
	      in
		()
	      end
	end (* Env *)

      val primitiveDecs: CoreML.Dec.t list = let
	    open CoreML.Dec
	    in
	      List.concat [
		  [Datatype primitiveDatatypes],
		  List.map (fn c => Exception{con = c, arg = NONE}) primitiveExcons
		]
	    end
    in
    fun addPrim E = (Env.addPrim E; primitiveDecs)
    end (* local *)


  (* ------------------------------------------------- *)
  (*                 parseAndElaborateMLB              *)
  (* ------------------------------------------------- *)

    fun quoteFile s = concat ["\"", (*String.escapeSML*)String.toString s, "\""]

    structure MLBString :> sig
	  type t

	  val fromFile: File.t -> t
	  val fromString: string -> t
	  val lexAndParseMLB: t -> Ast.Basdec.t
       end = struct
	  type t = string

	  val fromFile = quoteFile

	  val fromString = fn s => s

	  val lexAndParseMLB = MLBFrontEnd.lexAndParseString
       end

    val lexAndParseMLB = MLBString.lexAndParseMLB

    val lexAndParseMLB: MLBString.t -> Ast.Basdec.t = 
	  fn input => let
	      val ast = lexAndParseMLB input
	      val _ = Ctl.checkForErrors "parse"
	      in
		ast
	      end

    fun sourceFilesMLB {input} =
	  Ast.Basdec.sourceFiles (lexAndParseMLB (MLBString.fromFile input))

    val elaborateMLB = Elaborate.elaborateMLB

    val displayEnvDecs = Ctl.Layouts
	  (fn ((_, decs),output) =>
	   (output (Layout.str "\n\n")
	    ; MLtonVector.foreach
	      (decs, fn (dec, dc) =>
	       (output o Layout.record)
	       [("deadCode", (*Bool.layout dc*)Layout.str(Bool.toString dc)),
		("decs", MLtonList.layout CoreML.Dec.layout dec)])))

    fun parseAndElaborateMLB (input: MLBString.t) : Env.t * (CoreML.Dec.t list * bool) vector =
	  Ctl.pass {
	      display = displayEnvDecs,
	      name = "parseAndElaborate",
	      stats = fn _ => Layout.empty,
	      style = Ctl.ML,
	      suffix = "core-ml",
	      thunk = (fn () =>
		       (Const.lookup := lookupConstant
			; elaborateMLB (lexAndParseMLB input, {addPrim = addPrim})))
	    }


  (* ------------------------------------------------- *)
  (*                   Basis Library                   *)
  (* ------------------------------------------------- *)

    fun outputBasisConstants (out: Out.t): unit = let
	  val _ = amBuildingConstants := true
	  val (_, decs) = parseAndElaborateMLB (
		MLBString.fromFile "$(SML_LIB)/basis/primitive/primitive.mlb")
	  val decs = MVec.concatV (Vector.map (Vector.fromList o #1) decs)
	(* Need to defunctorize so the constants are forced. *)
	  val _ = Defunctorize.defunctorize (CoreML.Program.T {decs = decs})
	  val _ = LookupConstant.build (!allConstants, out)
	  in
	    ()
	  end


  (* ------------------------------------------------- *)
  (*                      compile                      *)
  (* ------------------------------------------------- *)

    exception Done

    fun elaborate {input: MLBString.t} : Xml.Program.t = let
	  val (E, decs) = parseAndElaborateMLB input
	  val _ = (case !Ctl.showBasis
		 of NONE => ()
		  | SOME f => File.withOut
		      (f, fn out => Layout.outputl (Env.layoutCurrentScope E, out))
		(* end case *))
	  val _ = Env.processDefUse E
	  val _ = if !Ctl.elaborateOnly then raise Done else ()
	  val decs = Ctl.pass {
		  display = Ctl.Layouts (
		      fn (decss, output) => (
			  output (Layout.str "\n\n");
			  Vector.app
			    (fn decs => List.app (fn dec => output (CoreML.Dec.layout dec)) decs)
			      decss)),
		  name = "deadCode",
		  suffix = "core-ml",
		  style = Ctl.ML,
		  stats = fn _ => Layout.empty,
		  thunk = fn () => #prog (DeadCode.deadCode {prog = decs})
		}
	  val decs = MVec.concatV (Vector.map Vector.fromList decs)
	  val coreML = CoreML.Program.T{decs = decs}
	  val _ = if !Ctl.keepCoreML
		    then Ctl.saveToFile (
		      {suffix = "core-ml"}, Ctl.No, coreML, Ctl.Layouts CoreML.Program.layouts)
		    else ()    
	  val xml = Ctl.passTypeCheck {
		  display = Ctl.Layouts Xml.Program.layouts,
		  name = "defunctorize",
		  stats = Xml.Program.layoutStats,
		  style = Ctl.ML,
		  suffix = "xml",
		  thunk = fn () => Defunctorize.defunctorize coreML,
		  typeCheck = Xml.typeCheck
		}
	  in
	    xml
	  end

    fun generateSXML {input: MLBString.t} = let
	  val xml = elaborate {input = input}
	  val xml = Ctl.passTypeCheck {
		  display = Ctl.Layouts Xml.Program.layouts,
		  name = "xmlSimplify",
		  stats = Xml.Program.layoutStats,
		  style = Ctl.ML,
		  suffix = "xml",
		  thunk = fn () => Xml.simplify xml,
		  typeCheck = Xml.typeCheck
		}
	  val _ = if !Ctl.keepXML
		then Ctl.saveToFile ({suffix = "xml"}, Ctl.No, xml, Ctl.Layouts Xml.Program.layouts)
		else ()
	  val sxml = Ctl.passTypeCheck {
		  display = Ctl.Layouts Sxml.Program.layouts,
		  name = "monomorphise",
		  stats = Sxml.Program.layoutStats,
		  style = Ctl.ML,
		  suffix = "sxml",
		  thunk = fn () => Monomorphise.monomorphise xml,
		  typeCheck = Sxml.typeCheck
		}
	  val sxml = Ctl.passTypeCheck {
		  display = Ctl.Layouts Sxml.Program.layouts,
		  name = "sxmlSimplify",
		  stats = Sxml.Program.layoutStats,
		  style = Ctl.ML,
		  suffix = "sxml",
		  thunk = fn () => Sxml.simplify sxml,
		  typeCheck = Sxml.typeCheck
		}
	  val _ = if !Ctl.keepSXML
		then Ctl.saveToFile ({suffix = "sxml"}, Ctl.No, sxml, Ctl.Layouts Sxml.Program.layouts)
		else ()
	  in
	    print "Completed generation of sxml\n";
	    sxml
	  end

    fun compileMLB {input: File.t} = generateSXML {input = MLBString.fromFile input}

    local
      fun genMLB {input: File.t list} : MLBString.t = let
	    val basis = "$(SML_LIB)/basis/default.mlb"
	  (* create an MLB string that compiles the files *)
	    val mlbS = (case input
		   of [] => basis
		    | _ => let
			val input = List.map quoteFile input
			in
			  String.concat [
			      "local\n",
			       basis, "\n",
			       "in\n",
			       String.concatWith "\n" input, "\n",
			       "end\n"
			    ]
			end
		  (* end case *))
	    in
	      MLBString.fromString mlbS
	    end
    in
    fun compilePML {input: File.t list} = generateSXML {input = genMLB {input = input}}
    end


  (* ------------------------------------------------- *)
  (*                  initialization                   *)
  (* ------------------------------------------------- *)

   fun parseMlbPathVar line = (case String.tokens Char.isSpace line
	  of [var, path] => SOME {var = var, path = path}
	   | _ => NONE
	 (* end case *))

    fun init () = let
	  val _ = Ctl.verbosity := Ctl.Pass
	  val _ = Ctl.keepSXML := true
	  val smlLibPath = concat["SML_LIB ", LoadPaths.libDir, "/mlton-basis/sml"]
	  val libMltonDir= concat["LIB_MLTON_DIR ", LoadPaths.libDir, "/mlton-basis/mlton"]
	  fun handlePath p = Control.mlbPathVars := !Control.mlbPathVars @ [
		  case parseMlbPathVar p
		   of NONE => Error.bug ("strange mlb path var: " ^ p)
		    | SOME v => v
		]
	  val _ = List.app handlePath [smlLibPath, libMltonDir]
	  in
	    ()
	  end

  end

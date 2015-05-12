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
    structure CoreBOM = CoreBOM (
      structure Ast = Ast)
    structure CoreML = CoreML (
	open Atoms
	structure Type =
	  struct
	    open TypeEnv.Type

	    val makeHom =
		 fn {con, var} => makeHom {con = con, expandOpaque = true, var = var}

	    fun layout t = layoutPrettyAux (t, {expandOpaque = true, localTyvarNames = false})
	   end
        structure CoreBOM = CoreBOM)
    structure Xml = Xml (
        open Atoms
        structure CoreBOM = CoreBOM)
    structure Sxml = Sxml (open Xml)
    structure Tycon = Atoms.Tycon

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
  structure CoreBOM = CoreBOM
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
  (*                 Lookup Constant                   *)
  (* ------------------------------------------------- *)

    val commandLineConstants: {name: string, value: string} list ref = ref []
    fun setCommandLineConstant (c as {name, value}) =
       let
	  fun make (fromString, control) =
	     let
		fun set () =
		   case fromString value of
		      NONE => Error.bug (concat ["bad value for ", name])
		    | SOME v => control := v
	     in
		set
	     end
(* [PML] -- for now, we do not support exnHistory --
	  val () =
	     case List.peek ([("Exn.keepHistory",
			       make (Bool.fromString, Control.exnHistory))],
			     fn (s, _) => s = name) of
		NONE => ()
	      | SOME (_,set) => set ()
*)
       in
	  MLtonList.push (commandLineConstants, c)
       end

    val allConstants: (string * ConstType.t) list ref = ref []
    val amBuildingConstants: bool ref = ref false

    val lookupConstant =
       let
	  val zero = Const.word (WordX.fromIntInf (0, WordSize.word32))
(*
	  val constantsFile = concat[LoadPaths.libDir, "/mlton-basis/mlton/constants"]
*)
	  val constantsFile = concat[LoadPaths.basisDir, "constants"]
	  val f =
	     Promise.lazy
	     (fn () =>
	      if !amBuildingConstants
		 then (fn ({name, default}, t) =>
		       let
			  (* Don't keep constants that already have a default value.
			   * These are defined by _command_line_const and set by
			   * -const, and shouldn't be looked up.
			   *)
			  val () =
			     if isSome default
				then ()
			     else MLtonList.push (allConstants, (name, t))
		       in
			  zero
		       end)
	      else (*(fn ({name:string, default: string option}, t) => zero)) *)
		  File.withIn
		      (constantsFile (*concat [!Control.libTargetDir, "/constants"]*), fn ins =>
		  LookupConstant.load (ins, !commandLineConstants)))
       in
	  fn z => f () z
       end


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
(*DEBUG*)val _ = print "parsing complete\n"
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
		MLBString.fromFile "$(SML_LIB)/sequential/primitive/primitive.mlb")
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
    (* DEBUG *) val _ = print "finished parseAndElaborateMLB\n"
	  val _ = (case !Ctl.showBasis
		 of NONE => ()
		  | SOME f => File.withOut
		      (f, fn out => Layout.outputl (Env.layoutCurrentScope E, out))
		(* end case *))
    (* DEBUG *) val _ = print "finished showing basis\n"
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
        (* DEBUG  val _ = print "starting to generate SXML\n" *)
	  val xml = elaborate {input = input}
    (* DEBUG *) val _ = print "finished elaborate\n"
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
	    val basis = "$(SML_LIB)/default.mlb"
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
(*
	  val smlLibPath = concat["SML_LIB ", LoadPaths.libDir, "/mlton-basis/sml"]
	  val libMltonDir= concat["LIB_MLTON_DIR ", LoadPaths.libDir, "/mlton-basis/mlton"]
*)
	  val smlLibPath = concat["SML_LIB ", LoadPaths.basisDir]
(* DEBUG *)val _ = print(concat["SML_LIB=\"", String.toString LoadPaths.basisDir, "\"\n"])
	  fun handlePath p = Control.mlbPathVars := !Control.mlbPathVars @ [
		  case parseMlbPathVar p
		   of NONE => Error.bug ("strange mlb path var: " ^ p)
		    | SOME v => v
		]
	  val _ = List.app handlePath [smlLibPath]
	  fun cvtSz sz = Bytes.toBits(Bytes.fromInt sz)
	  in
	  (* the sizes for the x86-64 target *)
	    Control.Target.setSizes {
		cint = cvtSz 4,
		cpointer = cvtSz 8,
		cptrdiff = cvtSz 8,
		csize = cvtSz 8,
		header = cvtSz 8,
		mplimb = cvtSz 8,
		objptr = cvtSz 8,
		seqIndex = cvtSz 8
	      }
	  end

  end

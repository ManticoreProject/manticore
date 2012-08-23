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
    structure Env = Env
    structure LookupConstant = LookupConstant (
	structure Const = Const
	structure ConstType = ConstType
	structure Ffi = Ffi)
    structure Monomorphise = Monomorphise (
	structure Xml = Xml
	structure Sxml = Sxml)

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
		then saveToFile ({suffix = "xml"}, Ctl.No, xml, Ctl.Layouts Xml.Program.layouts)
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
		then saveToFile ({suffix = "sxml"}, Ctl.No, sxml, Ctl.Layouts Sxml.Program.layouts)
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
			       String.concatWith "\n" input, "\n"
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

  end

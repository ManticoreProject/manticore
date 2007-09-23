(* match-dfa.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * TODO: implement hashed consing of test states, which will achieve
 * the "step-3" state merging.
 *)

structure MatchDFA : MATCH_DFA =
  struct

    datatype path
      = ROOT of Var.var
      | PATH of {
	parent : path,
	index : int,
	ty : Types.ty
      }

  (* extend a path: i is the child index, ty is the type of the location,
   * and v is the source variable being bound to the location.
   *)
    fun extendPath (path, i, ty) = PATH{parent = path, index = i, ty = ty}

  (* compare two paths *)
    fun comparePath (ROOT x, ROOT y) = Var.compare(x, y)
      | comparePath (ROOT _, _) = GREATER
      | comparePath (_, ROOT _) = LESS
      | comparePath (
	  PATH{parent=p1, index=i1, ...}, PATH{parent=p2, index=i2, ...}
	) = (case comparePath(p1, p2)
	   of EQUAL => Int.compare(i1, i2)
	    | order => order
	  (* end case *))

(*+DEBUG*)
    fun pathToString path = let
	  fun f (ROOT x, l) = concat(Var.toString x :: l)
	    | f (PATH{parent, index, ty}, l) =
		f (parent, "." :: Int.toString index :: l)
	  in
	    f (path, [])
	  end
(*-DEBUG*)

  (* return the type of the location specified by a path *)
    fun typeOfPath (ROOT x) = Var.monoTypeOf x
      | typeOfPath (PATH{ty, ...}) = ty

  (* mapping from source variables to the path of their binding site *)
    type var_map = path Var.Map.map

    datatype dfa = DFA of {
	arg : AST.var,
	root : state ref,
	err : state,
	final : state list ref,
	idCnt : word ref		(* for generating state IDs *)
      }

    and state = S of {
	refCnt : int ref,
	id : word,
	kind : state_kind
      }

    and state_kind
      = TEST of (path * (simple_pat * state) list)
      | BIND of (var_map * state)
      | FINAL of (Var.Set.set * AST.exp)
      | COND of (var_map * AST.exp * state * state)
      | ERROR

    and simple_pat
      = ANY
      | LIT of Literal.literal
      | CON of (AST.dcon * Types.ty list * path list)

    fun same (S{refCnt=a, ...}, S{refCnt=b, ...}) = (a = b)
    fun compare (S{id=a, ...}, S{id=b, ...}) = Word.compare(a, b)
    fun hash (S{id, ...}) = id

    fun toString (S{kind, ...}) = (case kind
	   of TEST(path, _) => concat["TEST(", pathToString path, ")"]
	    | BIND _ => "BIND"
	    | FINAL _ => "FINAL"
	    | COND _ => "COND"
	    | ERROR => "ERROR"
	  (* end case *))

    fun mkDFA arg = let
	  val err = S{refCnt = ref 0, id = 0w0, kind = ERROR}
	  in
	    DFA{
		arg = arg,
		root = ref err,
		err = err,
		final = ref [],
		idCnt = ref 0w1
	      }
	  end

  (* return the number of states in a dfa *)
    fun size (DFA{idCnt, ...}) = Word.toIntX(!idCnt)

    fun mkState (DFA{idCnt, ...}, kind) = let
	  val id = !idCnt
	  in
	    idCnt := id+0w1;
	    S{refCnt=ref 0, id=id, kind=kind}
	  end

  (* increment a state's reference count *)
    fun inc (S{refCnt, ...}) = refCnt := !refCnt+1

  (* construct a test state *)
    fun mkTest (dfa, testVar, arcs) = (
	  List.app (fn (_, q) => inc q) arcs;
	  mkState (dfa, TEST(testVar, arcs)))

  (* construct a bind state *)
    fun mkBind (dfa, vmap, q as S{kind=FINAL _, ...}) = (
	  inc q;
	  mkState (dfa, BIND(vmap, q)))
      | mkBind (_, _, q as S{kind=ERROR, ...}) = q
      | mkBind (_, _, q) =
	  raise Fail(concat["mkBind: not final/error (", toString q, ")"])

  (* construct a final state *)
    fun mkFinal (dfa as DFA{final, ...}, vmap, exp) = let
	  val q = mkState (dfa, FINAL(vmap, exp))
	  in
	    final := q :: !final; q
	  end

  (* construct a conditional-test state. *)
    fun mkCond (dfa, vmap, e, s1 as S{kind=FINAL _, ...}, s2) = (
	  inc s1;
	  inc s2;
	  mkState (dfa, COND(vmap, e, s1, s2)))
      | mkCond _ = raise Fail "mkCond: true branch not final"

  (* set the initial state *)
    fun setInitialState (DFA{root, ...}, q) = (
	  inc q;
	  root := q)

  (* get distinguished states from a DFA *)
    fun initialState (DFA{root, ...}) = !root
    fun errorState (DFA{err, ...}) = err
    fun finalStates (DFA{final, ...}) = !final

  (* get the argument variables of the DFA *)
    fun getArg (DFA{arg, ...}) = arg

  (* return the kind of a state *)
    fun kind (S{kind, ...}) = kind

  (* return the reference count of a state *)
    fun rCount (S{refCnt, ...}) = !refCnt

  (* return the reference count of the error state *)
    fun errorCount (DFA{err, ...}) = rCount err

  (* path variables bound by a simple pattern *)
    fun pathsOf (CON(_, _, paths)) = paths
      | pathsOf _ = []

  (* return a string representation of a simple pattern *)
    fun patToString ANY = "_"
      | patToString (LIT lit) = Literal.toString lit
      | patToString (CON(dc, _, [])) = DataCon.nameOf dc
      | patToString (CON(dc, _, arg::args)) = let
	  fun toList [] = [")"]
	    | toList (path::r) = "," :: pathToString path :: toList r
	  in
	    concat(DataCon.nameOf dc :: "(" :: pathToString arg
	      :: toList args)
	  end

  (* dump a DFA to a file *)
    local
      structure Q = Queue
      structure PP = TextIOPP

      datatype 'a stateQ = SQ of {q : state Q.queue, marked : word list ref}
      fun mkQueue s0 = let
	    val q = Q.mkQueue()
	    in
	      Q.enqueue(q, s0);
	      SQ{q = q, marked = ref []}
	    end
      fun insert (SQ{q, ...}, s) = Q.enqueue(q, s)
      fun getState (sq as SQ{q, marked}) =
	    if Q.isEmpty q
	      then NONE
	      else let
		val s as S{id, ...} = Q.dequeue q
		in
		  if (List.exists (fn id' => (id = id')) (!marked))
		    then getState sq
		    else (
		      marked := id :: !marked;
		      SOME s)
		end
    in
    fun dump (outStrm, DFA{arg, root, err, final, ...}) = let
	  val stateQ = mkQueue(!root)
	  val ppStrm = TextIOPP.openOut{dst = outStrm, wid = 120}
	  val str = PP.string ppStrm
	  val sp = PP.space ppStrm
(* FIXME --- should have a PP for the AST
	  val ppExp = PPTypedAST.ppExp {showLocs = false}
*)
	  fun ppExp (ppStrm, _) = str "<exp>"
	  fun ppStateId (S{id, ...}) = str("state"^Word.toString id)
	  fun ppVar x = str(Var.toString x)
	  fun ppVList vl = let
		fun pVar' vp = (str ","; sp 1; ppVar vp)
		in
		  PP.openBox ppStrm (PP.Abs 2);
		    case vl
		     of [] => str "{}"
		      | (item::r) => (
			  sp 1;
			  str "{";
			  ppVar item;
			  List.app pVar' r;
			  str "}")
		    (* end case *);
		  PP.closeBox ppStrm
		end
	  fun ppVSet vset = ppVList (Var.Set.listItems vset)
	  fun ppVMap vmap = let
		fun pVar' (vp, path) = (str ","; sp 1; ppVar vp; str "@"; str(pathToString path))
		in
		  PP.openBox ppStrm (PP.Abs 2);
		    case Var.Map.listItemsi vmap
		     of [] => str "{}"
		      | ((v, path)::r) => (
			  sp 1;
			  str "{";
			  ppVar v; str "@"; str(pathToString path);
			  List.app pVar' r;
			  str "}")
		    (* end case *);
		  PP.closeBox ppStrm
		end
	  fun ppArc (pat, q) = (
		PP.newline ppStrm;
		PP.openHBox ppStrm;
		  str(patToString pat);
		  sp 1; str "=>"; sp 1;
		  ppNextState q;
		PP.closeBox ppStrm)
	  and ppNextState q =
		if ((rCount q) > 1)
		  then (str "goto"; sp 1; ppStateId q; insert(stateQ, q))
		  else ppState(false, q)
	  and ppState (doLabel, q) = (
		PP.openHBox ppStrm;
		  if doLabel
		    then (
		      ppStateId q; str "["; str(Int.toString(rCount q)); str "]";
		      str ":"; sp 1)
		    else ();
		  case (kind q)
		   of TEST(v, arcs) => (
			str "test"; sp 1; str(pathToString v);
			PP.openVBox ppStrm (PP.Abs 2);
			  List.app ppArc arcs;
			PP.closeBox ppStrm)
		    | BIND(vmap, q) => (
			PP.openHOVBox ppStrm (PP.Abs 2);
			  PP.openHBox ppStrm;
			    str "bind"; sp 1;
			    ppVMap vmap; sp 1;
			    str "in";
			  PP.closeBox ppStrm;
			sp 1;
			  PP.openHBox ppStrm;
			    ppNextState q;
			  PP.closeBox ppStrm;
			PP.closeBox ppStrm)
		    | FINAL(bvs, e) => (
			str "final"; sp 1;
			PP.openHBox ppStrm;
			  ppVSet bvs; sp 1;
			  ppExp (ppStrm, e);
			PP.closeBox ppStrm)
		    | COND(vmap, e, s1, s2) => let
			fun pp (l, s) = (
			      PP.newline ppStrm;
			      PP.openHBox ppStrm;
				str l; sp 1; ppNextState s;
			      PP.closeBox ppStrm)
			in
			  PP.openHBox ppStrm;
			    str "if"; sp 1; ppVMap vmap; ppExp(ppStrm, e);
			  PP.closeBox ppStrm;
			  PP.openVBox ppStrm (PP.Abs 2);
			    pp ("then", s1);
			    pp ("else", s2);
			  PP.closeBox ppStrm
			end
		    | ERROR => str "error"
		  (* end case *);
		PP.closeBox ppStrm)
	  fun ppStates () = (case getState stateQ
		 of NONE => ()
		  | (SOME s) => (ppState(true, s); PP.newline ppStrm; ppStates())
		(* end case *))
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      str "********** DUMP DFA **********"; PP.newline ppStrm;
	      PP.openHBox ppStrm;
		str "arg = "; ppVar arg;
	      PP.closeBox ppStrm;
	      PP.newline ppStrm;
	      ppStates ();
	      str "**********  **********"; PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    TextIOPP.closeStream ppStrm
	  end
    end (* local *)

  end

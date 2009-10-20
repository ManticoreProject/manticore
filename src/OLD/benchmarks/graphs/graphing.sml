(* graphing.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Graphing : sig

    type run = {
	n : int,		(* number of processors; 0 = sequential implementation *)
	t : real		(* time in seconds *)
      }

    type benchmark = {
	name : string,		(* name of the benchmark program *)
	size : int,		(* size of the problem; used to compute throughput *)
	date : string,		(* data stamp *)
	comment : string,	(* additional identifying info (e.g., scheduler) *)
	runs : run list		(* list of runs; should include at least a sequential run *)
      }

  (* cumulative benchmark info *)
    datatype bmark_info = BI of {
	id : int,		(* index (starts at 0) *)
	name : string,		(* name of the benchmark program *)
	size : int,		(* size of the problem; used to compute throughput *)
	date : string,		(* data stamp *)
	comment : string,	(* additional identifying info (e.g., scheduler) *)
	runs : real list array	(* array of runs. Each run has a list of times *)
      }

    type cumulative_info = (int * bmark_info list)

    val toBenchmarkInfo : benchmark list -> cumulative_info

    val speedup    : cumulative_info -> string list -> string -> OS.Process.status
    val times      : cumulative_info -> string list -> string -> OS.Process.status
    val efficiency : cumulative_info -> string list -> string -> OS.Process.status
    val overhead   : cumulative_info -> string list -> string -> OS.Process.status
    val throughput : cumulative_info -> string list -> string -> OS.Process.status

  (* data filtering support *)
    val nameIs : string -> bmark_info -> bool
    val sizeIs : (int -> bool) -> bmark_info -> bool

    val filter : (bmark_info -> bool) list -> cumulative_info -> cumulative_info

  end = struct

    type run = {
	n : int,		(* number of processors; 0 = sequential implementation *)
	t : real		(* time in seconds *)
      }

    type benchmark = {
	name : string,		(* name of the benchmark program *)
	size : int,		(* size of the problem; used to compute throughput *)
	date : string,		(* data stamp *)
	comment : string,	(* additional identifying info (e.g., scheduler) *)
	runs : run list		(* list of runs; should include at least a sequential run *)
      }

  (* cumulative benchmark info *)
    datatype bmark_info = BI of {
	id : int,		(* index (starts at 0) *)
	name : string,		(* name of the benchmark program *)
	size : int,		(* size of the problem; used to compute throughput *)
	date : string,		(* data stamp *)
	comment : string,	(* additional identifying info (e.g., scheduler) *)
	runs : real list array	(* array of runs. Each run has a list of times *)
      }

    val maxNumProcs = 8		(* 8 processors is our currrent limit *)

    type cumulative_info = (int * bmark_info list)

  (* create the cumulative info from a list of benchmark records *)
    fun toBenchmarkInfo bmarks = let
	  val nProcs = ref 0
	  fun match (b : benchmark) (BI{name, size, ...}) =
		(#name b = name) andalso (#size b = size)
	  val info = ref []
	  fun mergeRun runs {n, t} = (
		nProcs := Int.max(!nProcs, n);
		Array.update(runs, n, t::Array.sub(runs, n)))
	  fun doBMark (b, i) = (case List.find (match b) (!info)
		 of NONE => let
		      val bi = BI{
			      id = i,
			      name = #name b,
			      size = #size b,
			      date = #date b,
			      comment = #comment b,
			      runs = let
				val runs = Array.array(maxNumProcs+1, [])
				in
				  List.app (mergeRun runs) (#runs b);
				  runs
				end
			    }
		      in
			info := bi :: !info;
			i+1
		      end
		  | SOME(BI{runs, ...}) => (List.app (mergeRun runs) (#runs b); i)
		(* end case *))
	  in
	    ignore (List.foldl doBMark 0 bmarks);
	    (!nProcs, List.rev (!info))
	  end

    local
      structure F = Format

      val colors = Vector.fromList [
	      "red",
	      "darkblue",
	      "purple",
	      "redorange",
	      "yellow",
	      "teal",
	      "brightblue",
	      "brightgreen"
	    ]

      val shapes = Vector.fromList [
	      "shape=square style=outline fillcolor=white",
	      "shape=triangle style=outline fillcolor=white",
	      "shape=diamond style=outline fillcolor=white",
	      "shape=downtriangle style=outline fillcolor=white",
	      "shape=circle style=spokes",
	      "shape=square style=fill",
	      "shape=triangle style=fill",
	      "shape=diamond style=fill",
	      "shape=downtriangle style=fill"
	    ]

    (* Path to Ploticus command *)
      val plPath = "/usr/local/bin/pl";
    
      fun env () = (case OS.Process.getEnv "PLOTICUS_PREFABS"
	     of SOME path => ["PLOTICUS_PREFABS=" ^ path]
	      | NONE => ["PLOTICUS_PREFABS=/usr/local/src/pl240src/prefabs"]
	    (* end case *))

      fun args (out, usrArgs) = ["-stdin", "-o", out] @ usrArgs

      fun cumulative outFn usrArgs out = let
	    val proc = Unix.executeInEnv (plPath, args (out, usrArgs), env())
	    val outS = Unix.textOutstreamOf proc
	    in
	      outFn outS;
	      TextIO.closeOut outS;
	      Unix.reap proc
	    end

    (* compute the average time and standard deviation *)
      fun avgTime [] = NONE
	| avgTime [t] = SOME(t, 0.0)
	| avgTime ts = let
	    val n = real(List.length ts)
	    val avg = (List.foldl (op +) 0.0 ts) / n
	    fun sq x = x*x
	    val stdDev = Math.sqrt((List.foldl (fn (x, s) => s + sq(x - avg)) 0.0 ts) /n)
	    in
	      SOME(avg, stdDev)
	    end
  
    (* get the sequential performance for a benchmark *)
      fun seqTime (BI{name, runs, ...}) = (case avgTime(Array.sub(runs, 0))
	     of NONE => raise Fail("missing sequential data for " ^ name)
	      | SOME(avg, _) => avg
	    (* end case *))

      fun plotArea {outS, nProcs, title, yLabel, yMax} = let
	    fun pr s = TextIO.output(outS, s)
	    fun prl s = pr (String.concat s)
	    fun prf (fmt, items) = pr(F.format fmt items)
	    in
	    (* ploting area *)
	      pr "#proc areadef\n";
	      pr "  rectangle: 0 1.0 5.5 5.5\n";
	      prf ("  xrange: 0 %d.5\n", [F.INT nProcs]);
	      prf ("  yrange: 0 %g\n", [F.REAL yMax]);
	      if (title <> "") then prl["  title: ", title, "\n"] else ();
	      pr "#endproc\n";
	    (* X axis *)
	      pr "#proc xaxis\n";
	      pr "  label: Number of Processors\n";
	      pr "  stubs: inc 1\n";
	      pr "  stubrange: 1\n";
	      pr "#endproc\n";
	    (* Y axis *)
	      pr "#proc yaxis\n";
	      prl ["  label: ", yLabel, "\n"];
	      pr "  stubs: inc\n";
	      pr "#endproc\n"
	    end

      fun plotLegend {outS, x, y} = let
	    fun pr s = TextIO.output(outS, s)
	    fun prf (fmt, items) = pr(F.format fmt items)
	    in
	      pr "#proc legend\n";
	      prf("  location: %g %g\n", [F.REAL x, F.REAL y]);
	      pr "  textdetails: size=8\n";
	      pr "#endproc\n"
	    end

      fun processData plotSeq f info = let
	    fun processBI (bi as BI{id, name, size, runs, ...}, (lo, hi, bis)) = let
		  val seq = seqTime bi
		  val data = let
			fun get (i, [], acc) = acc
			  | get (i, rs, acc) = let
			      fun mapi [] = []
				| mapi (t::r) = f (size, seq, i, t) :: mapi r
			      in
				if (i <> 0) orelse plotSeq
				  then (i, valOf(avgTime(mapi rs))) :: acc
				  else acc
			      end
			in
			  Array.foldri get [] runs
			end
		  val (lo, hi) = let
			fun f ((_, (y, sd)), (lo, hi)) =
			      (Real.min(lo, y-sd), Real.max(hi, y+sd))
			in
			  List.foldl f (lo, hi) data
			end
		  in
		    (lo, hi, {id = id, name = name, data = data}::bis)
		  end
	    in
	      List.foldr processBI (Real.posInf, Real.negInf, []) info
	    end

      fun plotCurve {outS, nProcs} {id, name, data} = let
	    val color = Vector.sub(colors, id)
	    fun pr s = TextIO.output(outS, s)
	    fun prl s = pr (String.concat s)
	    fun prf (fmt, items) = pr(F.format fmt items)
	    in
	    (* data *)
	      pr "#proc getdata\n";
	      pr "  data:\n";
	      List.app
		(fn (i, (v, sd)) => prf("    %d %g %g\n", [F.INT i, F.REAL v, F.REAL sd]))
		  data;
	      pr "#endproc\n";
	    (* lineplot *)
	      pr "#proc lineplot\n";
	      pr "  xfield: 1\n";
	      pr "  yfield: 2\n";
	      prl["  linedetails: color=", color, "\n"];
	      prl["  pointsymbol: ", Vector.sub(shapes, id), " color=", color, "\n"];
	      prl["  legendlabel: ", name, "\n"];
	      pr "  legendsampletype: line+symbol\n";
	      pr "#endproc\n"
	    end
    in

  (* plot speedup curves *)
    fun speedup (nProcs, info) usrArgs outFile = let
	  fun plot outS = let
		fun pr s = TextIO.output(outS, s)
		fun prf (fmt, items) = pr(F.format fmt items)
		val (lo, hi, bis) = processData false (fn (_, seq, _, t) => seq / t) info
		val plotBI = plotCurve {
			outS = outS,
			nProcs = nProcs
		      }
		val yMax = real nProcs + 0.5
		in
		  plotArea {
		      outS = outS,
		      nProcs = nProcs,
		      title = "",
		      yLabel = "Speedup",
		      yMax = yMax
		    };
		(* plot perfect speedup line *)
		  pr "#proc getdata\n";
		  pr "  data:\n";
		  pr "    0 0\n";
		  prf("    %g %g\n", [F.REAL yMax, F.REAL yMax]);
		  pr "#endproc\n";
		  pr "#proc lineplot\n";
		  pr "  xfield: 1\n";
		  pr "  yfield: 2\n";
		  pr "  linedetails: color=skyblue style=1\n";
		  pr "  legendlabel: Perfect speedup\n";
		  pr "#endproc\n";
		(* plot curves *)
		  List.app plotBI bis;
		  plotLegend {
		      outS = outS,
		      x = 1.0, y = 5.0
		    }
		end
	  in
	    cumulative plot usrArgs outFile
	  end

  (* plot normalized time curves *)
    fun times (nProcs, info) usrArgs outFile = let
	  fun plot outS = let
		fun pr s = TextIO.output(outS, s)
		fun prf (fmt, items) = pr(F.format fmt items)
		val (lo, hi, bis) = processData false (fn (_, seq, _, t) => t / seq) info
		val plotBI = plotCurve {
			outS = outS,
			nProcs = nProcs
		      }
		val yMax = 1.2
		in
		  plotArea {
		      outS = outS,
		      nProcs = nProcs,
		      title = "",
		      yLabel = "Normalized time",
		      yMax = yMax
		    };
		(* plot curves *)
		  List.app plotBI bis;
		  plotLegend {
		      outS = outS,
		      x = 1.0, y = 2.0
		    }
		end
	  in
	    cumulative plot usrArgs outFile
	  end

  (* plot parallel-efficiency curves *)
    fun efficiency (nProcs, info) usrArgs outFile = let
	  fun plot outS = let
		fun pr s = TextIO.output(outS, s)
		fun prf (fmt, items) = pr(F.format fmt items)
		val (lo, hi, bis) =
		      processData false (fn (_, seq, n, t) => (100.0 * seq / (real n * t))) info
		val plotBI = plotCurve {
			outS = outS,
			nProcs = nProcs
		      }
		val yMax = 110.0
		in
		  plotArea {
		      outS = outS,
		      nProcs = nProcs,
		      title = "",
		      yLabel = "Parallel efficiency (%)",
		      yMax = yMax
		    };
		(* plot curves *)
		  List.app plotBI bis;
		  plotLegend {
		      outS = outS,
		      x = 1.0, y = 2.0
		    }
		end
	  in
	    cumulative plot usrArgs outFile
	  end

  (* plot sequential-overhead curves *)
    fun overhead (nProcs, info) usrArgs outFile = let
	  fun plot outS = let
		fun pr s = TextIO.output(outS, s)
		fun prf (fmt, items) = pr(F.format fmt items)
		val (lo, hi, bis) =
		      processData false
			(fn (_, seq, n, t) => Real.max(0.0, 100.0 - (100.0 * seq / (real n * t))))
			  info
		val plotBI = plotCurve {
			outS = outS,
			nProcs = nProcs
		      }
		val yMax = 100.0
		in
		  plotArea {
		      outS = outS,
		      nProcs = nProcs,
		      title = "",
		      yLabel = "Sequential overhead (%)",
		      yMax = yMax
		    };
		(* plot curves *)
		  List.app plotBI bis;
		  plotLegend {
		      outS = outS,
		      x = 1.0, y = 5.0
		    }
		end
	  in
	    cumulative plot usrArgs outFile
	  end

  (* plot throughput curves *)
    fun throughput (nProcs, info) usrArgs outFile = let
	  fun plot outS = let
		fun pr s = TextIO.output(outS, s)
		fun prf (fmt, items) = pr(F.format fmt items)
		val (lo, hi, bis) = processData false (fn (sz, seq, n, t) => real sz / t) info
		val plotBI = plotCurve {
			outS = outS,
			nProcs = nProcs
		      }
		in
		  plotArea {
		      outS = outS,
		      nProcs = nProcs,
		      title = "",
		      yLabel = "Throughput",
		      yMax = hi
		    };
		(* plot curves *)
		  List.app plotBI bis;
		  plotLegend {
		      outS = outS,
		      x = 1.0, y = 5.0
		    }
		end
	  in
	    cumulative plot usrArgs outFile
	  end

  (* data filtering support *)
    fun nameIs s (BI{name, ...}) = (name = s)
    fun sizeIs tst (BI{size, ...}) = (tst size)

    fun filter preds (nProcs, info) =
	  (nProcs, List.filter (fn bi => List.all (fn f => f bi) preds) info)


    end (* local *)
  end

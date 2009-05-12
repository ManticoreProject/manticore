local
(* Path to Ploticus command *)
val plPath = "/usr/local/bin/pl";

val env = [
	"PLOTICUS_PREFABS=/usr/local/src/pl240src/prefabs"
      ];
in
(* average a sequence of measurements *)
fun average (ts : real list) = let
      val n = real(length ts)
      val s = List.foldl Real.+ 0.0 ts
      in
	s / n
      end
fun ploticus args outFn = let
      val proc = Unix.executeInEnv (plPath, args, env)
      val outS = Unix.textOutstreamOf proc
      in
	outFn outS;
	TextIO.closeOut outS;
	Unix.reap proc
      end
end;

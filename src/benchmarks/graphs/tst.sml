(* tst.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A simple test driver for the graphing support.
 *)

app use [
  "test-data.sml",
  "graphing.sml"
];

structure G = Graphing;

val info = G.toBenchmarkInfo data;

G.speedup info ["-eps"] "a.eps";
G.efficiency info ["-eps"] "b.eps";
G.overhead info ["-eps"] "c.eps";
G.times info ["-eps"] "d.eps";
G.throughput (G.filter [G.sizeIs (fn n => n > 1)] info) ["-eps"] "e.eps";

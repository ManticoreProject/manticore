(* leaf-size.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Determine the maximum number of data elements M that can be stored at rope leaves
 * (i.e., the max leaf size) via the command line.
 *)

structure LeafSize = struct
local
val dflt = 512
val maxR = ref 1
in
fun getMax () = !maxR
fun setMax max' = 
  if getMax () < 1 then 
    (raise Fail "invalid max leaf size" )
  else 
    maxR := max'
val _ = setMax (ParseCommandLine.parse1 "-max-leaf-size" Int.fromString dflt)
end
end

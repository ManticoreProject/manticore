signature ROPE_TY = sig
structure Seq : SEQ
datatype 'a rope
  = Leaf of 'a Seq.seq
  | Cat  of int * int * 'a rope * 'a rope 
         (* length, depth, left subrope, right subrope *)
end

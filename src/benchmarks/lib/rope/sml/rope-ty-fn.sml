functor RopeTyFn (
  structure Seq : SEQ
) : ROPE_TY =
struct
structure Seq = Seq
datatype 'a rope
  = Leaf of 'a Seq.seq
  | Cat  of int * int * 'a rope * 'a rope
end

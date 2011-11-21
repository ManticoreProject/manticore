structure IntRopeTy =
struct
structure Seq = IntSeq
datatype rope
  = Leaf of Seq.seq
  | Cat  of int * int * rope * rope
end

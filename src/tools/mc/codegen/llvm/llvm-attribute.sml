(* llvm-attribute.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * extremely simple attributes
 *)

structure LLVMAttribute = struct

  (* Add more as needed. It's important
  to check whether the instruction, parameter,
  or function actually recognizes the attribute
  you specify, because they only pay attention
  to ones that apply to it. For example, just because
  you specified "volatile" when generating an Add
  instruction doesn't mean such an instruction exists. *)

  datatype t
    = Atomic
    | Volatile
    | Aligned of int
    | NSW (* no signed wrap *)
    | NUW (* no unsigned wrap *)


  fun toString (x : t) : string = (case x
    of Atomic => "atomic"
     | Volatile => "volatile"
     | Aligned i => "aligned " ^ (Int.toString i)
     | NSW => "nsw"
     | NUW => "nuw"
    (* esac *))

  fun id (x : t) : int = (case x
    of Atomic => 1
     | Volatile => 2
     | Aligned _ => 3
     | NSW => 4
     | NUW => 5
    (* esac *))

  (* strings... gross *)
  structure K = struct
    type ord_key = t
    val compare = (fn (x, y) => Int.compare(id x, id y))
  end

    structure Set = RedBlackSetFn (K)

end

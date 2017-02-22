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
    | ExactDiv (* for sdiv NOTE this needs to be changed to just be "Exact" b/c other ops use it like lshr
                    don't rely on this for now! *)
    | Tail      (* for calls *)
    (* fast math flags for fadd, fsub, fmul, fdiv, frem, fcmp *)
    | NoNaN
    | NoInf
    | NoSZero
    | AllowRecip
    | FastMath  (* allows algebraically equiv transforms, also
                   implies all of the above flags *)
    (* atomic orderings *)
    | SeqCst

  
  val atomicOrderings = [ SeqCst ]


  fun toString (x : t) : string = (case x
    of Atomic => "atomic"
     | Volatile => "volatile"
     | Aligned i => "align " ^ (Int.toString i)
     | NSW => "nsw"
     | NUW => "nuw"
     | ExactDiv => "exact"
     | NoNaN        => "nnan"
     | NoInf        => "ninf"
     | NoSZero      => "nsz"
     | AllowRecip   => "arcp"
     | FastMath     => "fast"
     | SeqCst       => "seq_cst"
     | Tail         => "musttail"
    (* esac *))

  fun id (x : t) : int = (case x
    (* We fold all alignments into one element. If the alignment's value
        is desired, turn the set in which the alignment exists into a list
        and then match on the Aligned(i) which must exist there. Because
        the ID is 0, the list should have Aligned as the first element
        so this shouldn't be too expensive to do.
    *)
    of Aligned _    => 0
     | Atomic       => 1
     | Volatile     => 2 
     | NSW          => 3
     | NUW          => 4
     | ExactDiv     => 5
     | NoNaN        => 6
     | NoInf        => 7
     | NoSZero      => 8
     | AllowRecip   => 9
     | FastMath     => 10
     | SeqCst       => 11
     | Tail         => 12

     

    (* esac *))

  structure K = struct
    type ord_key = t
    val compare = (fn (x, y) => Int.compare(id x, id y))
  end

  structure Set = RedBlackSetFn (K)

end

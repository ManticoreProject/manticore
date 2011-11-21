signature SEQ =
sig

type 'a seq

val empty      : unit -> 'a seq
val isEmpty    : 'a seq -> bool
val length     : 'a seq -> int
val fromList   : 'a list -> 'a seq
val toList     : 'a seq -> 'a list
val singleton  : 'a -> 'a seq

val cat2       : 'a seq * 'a seq -> 'a seq
val catN       : 'a seq list -> 'a seq
(* split2 s *)
(* returns subsequences (s1, s2) of s such that length s1 *)
(* is equal to (or nearly equal to) length s2 *)
val split2     : 'a seq -> 'a seq * 'a seq
(* splitN (s, n) *)
(* splits s into n subsequences of equal size *)
(* raise Fail exception if length s < n *)
val splitN     : 'a seq * int -> 'a seq list

val take   : 'a seq * int -> 'a seq
val drop   : 'a seq * int -> 'a seq
(* insert (s, i, x) *)
(* inserts element x after the ith element of s *)
val insert : 'a seq * int * 'a -> 'a seq
(* sub (s, i) *)
(* returns element at index 0 <= i < (length s) of s *)
(* raises exception Subscript if i is not in bounds *)
(* e.g., subAtIx ([1,2], 1) ==> 1 *)
val sub    : 'a seq * int -> 'a

type ('a, 'b) progress = ('a, 'b) Progress.progress
(* similar to List.tabulate *)
(* e.g., tabulate (3, fn x => x) ==> [0,1,2] *)
val tabulate : int * (int -> 'a) -> 'a seq
(* tabulateUntil cond (i, j, f) *)
(* similar to tabulate above, except that the operation is pausable *)
(* e.g., tabulateUntil (fn _ => false) (1,3,fn x => x) ==> [1,2] *)
val tabulateUntil : (unit -> bool) -> ((int * int) * (int -> 'a))
                      -> ('a seq, 'a seq) progress
val map : ('a -> 'b) -> 'a seq -> 'b seq
val mapUntil : (unit -> bool) -> ('a -> 'b) -> 'a seq 
               -> ('a seq * 'b seq, 'b seq) progress
val reduce : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a
val reduceUntil : (unit -> bool) -> ('a * 'a -> 'a) -> 'a -> 'a seq
                  -> ('a * 'a seq, 'a) progress
val scan : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq
(* returns values of the form (acc, prog), where the value *)
(* acc contains the result of *)
(* f (ps_0, f (ps_1, ..., f (ps_n, acc))), where n is the length of *)
(* the input sequence ps *)
val scanUntil : (unit -> bool) -> ('a * 'a -> 'a) -> 'a -> 'a seq
                  -> ('a * ('a seq * 'a seq, 'a seq) progress)
val filter : ('a -> bool) -> 'a seq -> 'a seq
val filterUntil : (unit -> bool) -> ('a -> bool) -> 'a seq 
                  -> ('a seq * 'a seq, 'a seq) progress

val app : ('a -> unit ) -> 'a seq -> unit

end

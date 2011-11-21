signature ROPE =
sig

type 'a rope

(* O(1) empty rope *)
val empty      : unit -> 'a rope
(* O(1) tests whether a rope is empty *)
val isEmpty    : 'a rope -> bool
(* O(1) length of a rope *)
val length     : 'a rope -> int
(* O(n) conversion from a list to a rope *)
val fromList   : 'a list -> 'a rope
(* O(n) conversion from a rope to a list *)
val toList     : 'a rope -> 'a list
(* O(1) singleton rope *)
val singleton  : 'a -> 'a rope

(* O(log n) concatenates two ropes *)
val cat2       : 'a rope * 'a rope -> 'a rope
(* O(m log n) concatenates m ropes *)
val catN       : 'a rope list -> 'a rope
(* O(1) splits a rope into two subropes of equal size *)
val split2     : 'a rope -> 'a rope * 'a rope
(* O(m) splits a rope into m pieces of equal size *)
val splitN     : 'a rope * int -> 'a rope list

(* take (rp, i) *)
(* O(log n) returns the first i elements of the rope rp *)
(* it raises Subscript if i < 0 or i >= length l *)
(* we have take (l, length l) = l *)
val take   : 'a rope * int -> 'a rope
(* drop (rp, i) *)
(* O(log n) returns what is left after dropping the first i *)
(* elements of rope rp *)
(* it raises Subscript if i < 0 or i >= length l *)
(* it holds that cat2 (take(rp, i), drop(l, i)) = rp when *)
(* 0 <= i <= length rp. *)
(* we also have drop(rp, length rp) = empty (). *)
val drop   : 'a rope * int -> 'a rope
(* sub (rp, i) *)
(* O(log n) returns element at index 0 <= i < (length rp) of rp *)
(* raises exception Subscript if i is not in bounds *)
val sub    : 'a rope * int -> 'a

(* tabulate (n, f) *)
(* O(log n) returns a rope of length n equal to *)
(* [f 0, f 1, ..., f (n - 1)] *)
(* it raises Size if n < 0 *)
val tabulate : int * (int -> 'a) -> 'a rope
(* map f rp *)
(* O(log n) applies f to each element of rp, returning the rope *)
(* of results *)
val map : ('a -> 'b) -> 'a rope -> 'b rope
(* reduce f aop rp *)
(* O(log n) rope reduction for associative operator f and unit z *)
(* returns z aop rp1 aop rp2 aop ... aop rpn *)
val reduce : ('a * 'a -> 'a) -> 'a -> 'a rope -> 'a
(* scan f aop rp *)
(* O(log n) returns all partial sums of rope rp w.r.t. associative *)
(* operator f and unit z *)
(* returns [z, z aop rp1, z aop rp1 aop rp2, ..., z aop rp1 aop ... aop rpn] *)
val scan : ('a * 'a -> 'a) -> 'a -> 'a rope -> 'a rope
(* filter f rp *)
(* O(log n) applies f to each element of rp, returning the rope of those x *)
(* for which f x evaluated to true, in the same order as they occurred in the *)
(* argument rope *)
val filter : ('a -> bool) -> 'a rope -> 'a rope

val app : ('a -> unit) -> 'a rope -> unit

end

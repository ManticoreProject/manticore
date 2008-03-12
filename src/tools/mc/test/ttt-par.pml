datatype player = X | O;

type board = player option list (* of length 9 *)

  (* 0 1 2
     3 4 5
     6 7 8 *);

val empty : board = NONE::NONE::NONE::NONE::NONE::NONE::NONE::NONE::NONE::nil;

fun isSome(x) = 
  case x 
    of SOME _ => true 
     | NONE => false;

(* isOccupied : board * int -> bool *)
fun isOccupied (b,i) = isSome(nth(b,i));

(* isEmpty : board * int -> bool *)
fun isEmpty (b, i) = not (isOccupied (b, i));

(* find : board * int -> player option *)
fun find (b, i) = nth (b, i);

(* playerEq : player * player -> bool *)
fun playerEq tup =
  case tup
    of (X, X) => true
     | (O, O) => true
     | _ => false;

(* playerOccupies : player * board * int -> bool *)
fun playerOccupies (p, b, i) =
  case find (b, i)
    of SOME(q) => playerEq (p, q)
     | NONE => false;

(* hd : 'a list -> 'a *)
fun hd L =
  case L
    of x::_ => x
     | nil => raise Fail "empty";

(* tl : 'a list -> 'a list *)
fun tl L =
  case L
   of _::xs => xs
    | nil => raise Fail "empty";

(* putAt : 'a * 'a list * int -> 'a list *)
fun putAt (x, xs, i) =
  if (i=0) then (x :: tl(xs))
  else if (i>0) then (hd(xs)::putAt(x,tl(xs),i-1))
  else raise Fail "negative";

(* moveTo : board * player * int -> board *)
fun moveTo (b, p, i) =
  if isOccupied(b,i) then raise Fail "illegal move"
  else putAt (SOME(p), b, i);

  (* 0 1 2
     3 4 5
     6 7 8 *)

val rows = [| [| n, n+1, n+2 |] | n in [| 0, 3, 6 |] |];

val cols = [| [| n, n+3, n+6 |] | n in [| 0 to 2 |] |];

val diags = [| [| 0, 4, 8 |], [| 2, 4, 6 |] |];

(*
(* all : ('a -> bool) * 'a list -> bool *)
(* *** FIXME *** error in case simplify on this version of "all". *)
(* When the function is written as below (uncommented), it goes through.
 * See the file list-all.pml which is an as-yet unsuccessful attempt
 * to isolate this bug. -ams
 *)
fun all (pred, xs) = let
  fun f arg = (case arg
          of nil => true
           | x::xs => pred(x) andalso f(xs))
  in
    f xs
  end;
*)

fun all (pred, xs) = case xs
  of nil => true
   | h::t => pred(h) andalso all(pred,t);

(* allP : ('a -> bool) * 'a parray -> bool *)
fun allP (pred, xs) = let
  fun and' (b1, b2) = b1 andalso b2
  in
    reduceP (and', true, [| pred x | x in xs |])
  end;

(* existsP : ('a -> bool) * 'a parray -> bool *)
fun existsP (pred, xs) = let
  fun or' (b1, b2) = b1 orelse b2
  in
    reduceP (or', false, [| pred x | x in xs |])
  end;

(* hasTriple : board * player -> int parray -> bool *)
fun hasTriple (b, p) = let
  fun occ i = playerOccupies (p, b, i)
  fun hasIt t = allP (occ, t)
  in 
    hasIt
  end;

(* hasRow : board * player -> bool *)
fun hasRow (b, p) = existsP (hasTriple (b, p), rows);

(* hasCol : board * player -> bool *)
fun hasCol (b, p) = existsP (hasTriple (b, p), cols);

(* hasDiag : board * player -> bool *)
fun hasDiag (b, p) = existsP (hasTriple (b, p), diags);

(* isFull : board -> bool *)
fun isFull b = all (isSome, b);

(* PCASE OPPORTUNITIES *)
(* isWinFor : board * player -> bool *)
fun isWinFor (b, p) = hasRow(b,p) orelse hasCol(b,p) orelse hasDiag(b,p);

(* isWin : board -> bool *)
fun isWin b = isWinFor (b, X) orelse isWinFor (b, O);

(* isCat : board -> bool *)
fun isCat b = isFull(b) andalso not (isWinFor (b, X)) 
                        andalso not (isWinFor (b, O));

(* gameOver : board -> bool *)
fun gameOver b = isWin(b) orelse isCat(b);

(* score : board -> int *)
(* -1 if O wins, 1 if X wins, 0 otherwise. *)
(* This coarse heuristic function suffices b/c we can build the *whole* tree. *)
fun score b = 
  if isWinFor (b, X) then 1 
  else if isWinFor (b, O) then ~1 
  else 0;

datatype tttTree          (* monomorphic b/c there were bugs *)
  = Rose of ((board * int) * (tttTree parray));

(* mkLeaf : board * int -> tttTree *)
fun mkLeaf (b, i) = Rose ((b, i), [||])

(*
(* allMoves : board -> int parray *)
fun allMoves b = let 
  fun f (n, L, acc) =
    (case L
      of nil => acc
       | SOME(_)::more => f (n+1, more, acc)
       | NONE::more => f (n+1, more, pappend (acc, [|n|])))
  in
    f (0, b, [||])
  end;
*)

fun allMoves b = let
  fun f (n, L, acc) = (case L
    of nil => acc
     | SOME(_)::more => f (n+1, more, acc)
     | NONE::more => f (n+1, more, n::acc))
  val empties = f (0, b, nil)
  in
    case empties
     (* This case does what filter would do if we had it! *)
     of a::b::c::d::e::f::g::h::i::nil => [|a,b,c,d,e,f,g,h,i|]
      | a::b::c::d::e::f::g::h::nil => [|a,b,c,d,e,f,g,h|]
      | a::b::c::d::e::f::g::nil => [|a,b,c,d,e,f,g|]
      | a::b::c::d::e::f::nil => [|a,b,c,d,e,f|]
      | a::b::c::d::e::nil => [|a,b,c,d,e|]
      | a::b::c::d::nil => [|a,b,c,d|]
      | a::b::c::nil => [|a,b,c|]
      | a::b::nil => [|a,b|]
      | a::nil => [|a|]
      | nil => [||]
      | _ => fail "shouldn't"
  end;

(* other : player -> player *)
fun other p =
  case p 
    of X => O 
     | O => X;

(* top : tttTree -> 'a *)
fun top t = case t
  of Rose (x, _) => x;

(* add : int * int -> int *)
fun add (m:int, n) = m+n;

(* sumP : int parray -> int *)
fun sumP ns = reduceP (add, 0, ns);

(* size : 'a rose_tree -> int *)
fun size (t : tttTree) = case t
  of Rose (_, ts) => (sumP [| size t | t in ts |]) + 1;

(* min : int * int -> int *)
fun min (m:int, n) = (if m<n then m else n);

(* max : int * int -> int *)
fun max (m:int, n) = (if m>n then m else n);

(* minP : int parray -> int *)
fun minP a = reduceP (min, 2, a); (* 2 is INF in this domain! *)

(* maxP : int parray -> int *)
fun maxP a = reduceP (max, ~2, a);
 
(* select2 : 'a * 'b -> 'b *)
fun select2 (_, s) = s;

(* successors : board * player -> board parray *)
fun successors (b, p) = [| moveTo (b, p, i) | i in (allMoves b) |];

(* p is the player to move *)
(* X is max, O is min *)
fun minimax (b : board, p : player) =
  if gameOver(b) then
    mkLeaf (b, score b)
  else let
    val trees  = [| minimax (b, other p) | b in (successors (b, p)) |]
    val scores = [| select2(top(t)) | t in trees |]
    val selectFrom = (case p of X => maxP | O => minP)
    in
      Rose ((b, selectFrom scores), trees)
    end;

val t0 = gettimeofday();
val T  = minimax (empty, X);
val t1 = gettimeofday();

val (b, result) = top(T);

val t2 = gettimeofday();
val s = size(T);
val t3 = gettimeofday();

val build_tree_time = t1 - t0;

val size_time = t3 - t2; 

(print ("The outcome of the game is ");
 print (itos(result));
 print " (expecting 0).\nThe size of T is ";
 print (itos(s));
 print " (expecting 549946).\n";
 print "Time to build the tree: ";
 print (dtos(build_tree_time));
 print "\nTime to compute the size of the tree: ";
 print (dtos(size_time));
 print "\n")

(* FIXME ==>

fun successorsSeq (b, p) = let
  fun m i = moveTo (b, p, i)
  in
    map (m, allMoves b)
  end;

fun alpha_beta () = let
  fun maxT (board, alpha, beta) =
    if gameOver(board) then
      mkLeaf (board, score board)
    else let
      fun loop (bs, a, ts) = (case bs
        of nil => Rose ((board, a), ts)
	 | bh::bt => let
             val t = minT (bh, a, beta)
	     val a' = max (a, select2 (top t))
	     in
               if (a' >= beta) then
                 Rose ((board, a'), t::ts)
	       else
		 loop (bt, a', t::ts)
             end)
        in
	  loop (successorsSeq (board, X), alpha, nil)
	end
  and minT (board, alpha, beta) =
    if gameOver(board) then
      mkLeaf (board, score board)
    else let
      fun loop (bs, b, ts) = (case bs
        of nil => Rose ((board, b), ts)
	 | bh::bt => let
             val t = maxT (bh, alpha, b)
	     val b' = min (b, select2 (top t))
	     in
               if (b' <= alpha) then
                 Rose ((board, b'), t::ts)
	       else
		 loop (bt, b', t::ts)
             end)
        in
	  loop (successorsSeq (board, O), beta, nil)
	end
  in
    maxT (empty, ~1, 1)
  end;
*)

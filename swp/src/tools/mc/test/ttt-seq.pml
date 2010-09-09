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
     | (X, O) => false (* NOTE This function was broken with a wildcard for false. *)
     | (O, X) => false;

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

val rows  = ((0::1::2::nil)::(3::4::5::nil)::(6::7::8::nil)::nil);

val cols  = ((0::3::6::nil)::(1::4::7::nil)::(2::5::8::nil)::nil);

val diags = ((0::4::8::nil)::(2::4::6::nil)::nil);

(* all : ('a -> bool) * 'a list -> bool *)
fun all (pred, xs) =
  case xs
    of nil => true
     | x::xs => (pred x) andalso (all (pred, xs));

(* exists : ('a -> bool) * 'a list -> bool *)
fun exists (pred, xs) =
  case xs
    of nil => false
     | x::xs => (pred x) orelse (exists (pred, xs));

(* hasTriple : board * player -> int list -> bool *)
fun hasTriple (b, p) = let
  fun occ i = playerOccupies (p, b, i)
  fun hasIt t = all (occ, t)
  in 
    hasIt
  end;

(* hasRow : board * player -> bool *)
fun hasRow (b, p) = exists (hasTriple (b, p), rows);

(* hasCol : board * player -> bool *)
fun hasCol (b, p) = exists (hasTriple (b, p), cols);

(* hasDiag : board * player -> bool *)
fun hasDiag (b, p) = exists (hasTriple (b, p), diags);

(* isFull : board -> bool *)
fun isFull b = all (isSome, b);

(* isWinFor : board * player -> bool *)
fun isWinFor (b, p) = hasRow(b,p) orelse hasCol(b,p) orelse hasDiag(b,p);

(* isWin : board -> bool *)
fun isWin b = isWinFor (b, X) orelse isWinFor (b, O);

(* isCat : board -> bool *)
fun isCat b = isFull(b) andalso not (isWinFor (b, X)) 
                        andalso not (isWinFor (b, O));

(* score : board -> int *)
(* -1 if O wins, 1 if X wins, 0 otherwise. *)
(* This coarse heuristic function suffices b/c we can build the *whole* tree. *)
fun score b = 
  if isWinFor (b, X) then 1 
  else if isWinFor (b, O) then ~1 
  else 0;

(*
datatype 'a gtree (* general tree *)
  = Leaf of 'a
  | Node of 'a * ('a gtree list);
*)

datatype tttTree
  = Rose of ((board * int) * (tttTree list));

(* mkLeaf : board * int -> tttTree *)
fun mkLeaf (b, i) = Rose ((b, i), nil);

(* allMoves : board -> int list *)
fun allMoves b = let 
  fun f (n, L, acc) =
    case L
      of nil => rev acc
       | SOME(_)::more => f (n+1, more, acc)
       | NONE::more => f (n+1, more, n::acc)
  in
    f (0, b, nil)
  end;

(* other : player -> player *)
fun other p =
  case p 
    of X => O 
     | O => X;

(* top : 'a gtree -> 'a *)
fun top t = case t
  of Rose (x, ts) => x;

(*
(* immChildren : 'a gtree -> 'a list *)
fun immChildren t =
  case t
    of Leaf _ => nil (* raise Fail "" *)
     | Node (_, ts) => map (top, ts);
*)

(* add : int * int -> int *)
fun add (m:int, n) = m+n;

(* sum : int ilst -> int *)
fun sum L = foldl (add, 0, L);

(* size : 'a gtree -> int *)
fun size (t : tttTree) = case t
  of Rose (_, ts) => (sum (map (size, ts))) + 1;

(* BUGS? The following doesn't make it past the translate phase.
         Nor does it make it without the type ascription on t.
(* size : 'a gtree -> int *)
fun size (t : tttTree) =
  case t
    of Leaf _ => 1
     | Node (_, ts) => let
         val s = raise Fail "sum (map (size, ts))"
         in 
           s + 1
         end;
*)

(* listExtreme : (('a * 'a) -> 'a) * 'a list -> 'a *)
fun listExtreme (e, L) =
  case L
    of n::ns => foldl (e, n, ns)
     | nil => raise Fail "undefined on empty lists";

(* min : int * int -> int *)
fun min (m:int, n) = (if m<n then m else n);

(* max : int * int -> int *)
fun max (m:int, n) = (if m<n then n else m);

(* listmin : int list -> int *)
fun listmin (L : int list) = listExtreme (min, L);
 
(* listmax : int list -> int *)
fun listmax (L : int list) = listExtreme (max, L);

(* p is the player to move *)
(* X is max, O is min *)
fun minimax (b : board, p : player) =
  if (isWin(b) orelse isCat(b)) then
    mkLeaf (b, score b)
  else let
    fun moveTo' i = moveTo (b, p, i)
    fun minimax' b = minimax (b, other p)
    fun select2 (_, s) = s
    val moves = map moveTo' (allMoves b)
    val trees = map minimax' moves
    val scores = map (select2 o top) trees
    in
      case p
        of X => Rose ((b, listmax scores), trees)
	 | O => Rose ((b, listmin scores), trees)
    end;

val t0 = gettimeofday();
val T  = minimax (empty, X);
val t1 = gettimeofday();

val (b, result) = top(T);

val t2 = Time.now();
val s = size(T);
val t3 = Time.now();

val build_tree_time = t1 - t0;

val size_time = t3 - t2; 

val _ = (
      print ("The outcome of the game is ");
      print (Int.toString(result));
      print " (expecting 0).\nThe size of T is ";
      print (Int.toString(s));
      print " (expecting 549946).\n";
      print "Time to build the tree: ";
      print (Time.toString(build_tree_time));
      print "\nTime to compute the size of the tree: ";
      print (Time.toString(size_time));
      print "\n")

(* A bunch of debugging follows. *)

(*
fun btos b = if b then "true" else "false";

fun println s = (print s; print "\n");

fun otos o = (case o of NONE => "NONE" | SOME _ => "SOME");

fun Ltos ns = concatWith(",",map(Int.toString,ns));

fun Btos b = let
  fun str op =
    case op
      of NONE => " "
       | SOME X => "X"
       | SOME O => "O"
  in
    concatWith(",", map (str, b))
  end;

val sqs = 0::1::2::3::4::5::6::7::8::nil;

val allX = let fun f _ = SOME X in map(f,sqs) end;

val tree0 = Node ((nil, 1), Leaf(nil,1)::Leaf(nil,1)::nil);

val aBoard = SOME(X)::SOME(X)::NONE::SOME(O)::SOME(O)::SOME(O)::NONE::NONE::NONE::nil;

val aTree = minimax(aBoard,X);

(*
let fun str (b, s) = "(" ^ Btos(b) ^ "," ^ Int.toString(s) ^ ")"
in
  app(compose(println,str),immChildren(aTree))
end  
*)

(*
let fun occ i = playerOccupies(X,aBoard,i)
in
    app(compose(println,btos),map(occ,sqs))
end
*)

fun addMap (ms, ns) =
  case ms
   of nil => ns
    | m::mtl => (case ns
        of nil => ms
         | n::ntl =>
             (m+n)::addMap(mtl,ntl));

fun pathLengths t =
  (case t
    of Leaf b => (1 :: nil)
     | Node (b, ts) => 0 :: (foldl(addMap,nil,map(pathLengths,ts))));

fun zip2 (xs, ys) =
  (case xs
    of nil => (case ys
         of nil => nil
          | yh::yt => fail "")
     | xh::xt => (case ys
         of nil => fail ""
          | yh::yt => (xh,yh) :: zip2(xt,yt)));

fun boardEq (b1, b2) = let
  fun same (a, b) =
        (case a
          of NONE => (case b
               of NONE => true
                | SOME q => false)
           | SOME p => (case b
               of NONE => false
                | SOME q => playerEq (p, q))) 
  in
    all (same, zip2 (b1, b2))
  end;

fun mem (x, s) =
  case s
   of nil => false
    | e::es => boardEq(x,e) orelse mem(x,es);

fun ins (x, s) = if mem(x,s) then s else (x::s);

fun union (s1, s2) = foldl(ins, s1, s2);

fun foo arg = if isSome(arg) then 1 else 0;

val wins5 = let
  fun ct b = sum (map (foo, b))
  fun accrue arg = (case arg
        of (s, Leaf(b,i)) => if ct(b)=5 then ins(b,s) else s
         | (s, Node(b,ts)) => let
             fun accrue' t = accrue (s, t)
             in
               foldl (union, nil, map (accrue', ts))
             end)
  in
    accrue (nil, T)
  end;

app(compose(println,Btos),wins5)
*)

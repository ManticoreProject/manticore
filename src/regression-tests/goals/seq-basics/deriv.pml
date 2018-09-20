(* adapted from the Larceny benchmark

;;; DERIV -- Symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.
*)


datatype expr
  = Add of expr list
  | Sub of expr list
  | Mul of expr list
  | Div of expr list
  | Num of int
  | X

structure Benchmark = struct

val hd = List.hd
val tl = List.tl
val map = List.map
val app = List.app
val foldl = List.foldl

fun prnt p = (case p
  of X => print "x"
   | Num a => print (Int.toString a)
   | Add xs => prntLst "+" xs
   | Sub xs => prntLst "-" xs
   | Mul xs => prntLst "*" xs
   | Div xs => prntLst "/" xs
  (* end case *))
and prntLst symb xs = (print "("; print symb;
                       app prnt xs; print ")\n")

fun equal p = (case p
  of (X, X) => true
   | (Num a, Num b) => a = b
   | (Add xs, Add ys) => chkLists (xs, ys)
   | (Sub xs, Sub ys) => chkLists (xs, ys)
   | (Mul xs, Mul ys) => chkLists (xs, ys)
   | (Div xs, Div ys) => chkLists (xs, ys)
   | _ => false
  (* end case *))
and chkLists p = ListPair.foldlEq chk true p
and chk (a, b, acc) = acc andalso equal (a, b)

fun deriv a = (case a
  of X => Num 1
   | Num _ => Num 0
   | Add es => Add (map deriv es)
   | Sub es  => Sub (map deriv es)
   | Mul es  => Mul [
            a,
            Add (map (fn x => Div [deriv x, x]) es)
            ]

   | Div (x :: y :: nil) => Sub [
        Div [deriv x, y],
        Div [x, Mul [y, y, deriv y]]
      ]
   | _ => raise Fail "No derivation method available"
  (* end case *))


fun go (ans, exp) = let
  val computed = deriv exp
  in
    if equal (ans, computed)
    then ()
    else (prnt computed ; raise Fail "wrong answer!")
  end

end


structure Main =
  struct

  	val iterations = 20

    fun main _ = let

      fun mkExp a b =
        Add [
          Mul [Num 3, X, X],
          Mul [a, X, X],
          Mul [b, X],
          Num 5
        ]

      fun mkAns a b =
        Add [
          Mul [
            Mul [Num 3, X, X],
            Add [
              Div [Num 0, Num 3],
              Div [Num 1, X],
              Div [Num 1, X]
            ]
          ],
          Mul [
            Mul [a, X, X],
            Add [
              Div [Num 0, a],
              Div [Num 1, X],
              Div [Num 1, X]
            ]
          ],
          Mul [
            Mul [b, X],
            Add [
              Div [Num 0, b],
              Div [Num 1, X]
            ]
          ],
          Num 0
        ]

      val a = Num 5
      val b = Num 7
      val exp = mkExp a b
      val ans = mkAns a b

      fun doit () = Benchmark.go (ans, exp)

      fun lp 0 = ()
      	| lp n = (doit(); lp (n-1))

      fun start () = lp iterations

  	in
      	start ()
  	end

end

val _ = (Main.main () ; print "done\n")

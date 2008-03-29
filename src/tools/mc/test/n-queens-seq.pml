val seqSz = 1;

fun timeToEval (f) = let
    val b = gettimeofday ()
    val v = f()
    val e = gettimeofday ()
    in
       print (dtos (e-b)^"\n");
       v
    end
;

(*
;;; NQUEENS -- Compute number of solutions to 8-queens problem.

(define trace? #f)

(define (nqueens n)

  (define (dec-to n)
    (let loop ((i n) (l '()))
      (if (= i 0) l (loop (- i 1) (cons i l)))))

  (define (try x y z)
    (if (null? x)
      (if (null? y)
        (begin (if trace? (begin (write z) (newline))) 1)
        0)
      (+ (if (ok? (car x) 1 z)
           (try (append (cdr x) y) '() (cons (car x) z))
           0)
         (try (cdr x) (cons (car x) y) z))))

  (define (ok? row dist placed)
    (if (null? placed)
      #t
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

  (try (dec-to n) '() '()))

(time (do ((i 1000 (- 1 1))) ((zero? i)) (nqueens 8)))

*)

exception Foo of int

fun isOK (row, dist, placed) = (case placed
    of nil => true
     | p :: ps => p <> row+dist andalso p <> row-dist andalso isOK(row, dist+1, ps)
    (* end case *))
;

fun try (x, y, z) = (case x
    of nil => (case y
       of nil => (raise Foo 1)
	| _ => NONE
       (* end case *))
     | x :: xs => let
	   val l = if (isOK(x, 1, z))
		   then try(xs@y, nil, x :: z)
		   else NONE
           in
	      case l
	       of NONE => try(xs, x::y, z)
		| _ => l
           end
    (* end case *))
;

fun queens (n) = let
    fun f (i) = i
    fun doit () = let
	 val v = try(rev(tab(f, 0, n, 1)), nil, nil)	    
          in
             case v
	      of NONE => print "error\n"
	       | _ => ()
         end
    val t1 = gettimeofday()
    in
      doit()  handle _ => print (dtos (gettimeofday() - t1))
    end
;

queens(readint())

(*
signature PRINT = 
  sig
    val 'a withSep : (string * ('a -> string) * ('a list)) -> unit
    val int : int -> unit
    val ints : int list -> unit
    val ln : string -> unit
  end
*)

structure Print (* : PRINT *) = 
  struct

    fun withSep (sep, mkS, xs) = let
      fun w (ys, acc) =
       (case ys
         of y::ys => w (ys, acc ^ sep ^ mkS y) 
	  | nil => acc
        (* end case *))
      val s = "hi" (*
       (case xs
          of nil => ""
           | x::nil => mkS x
           | x::xs => w (xs, mkS x)
         (* end case *)) *)
      in
        print s
      end 

    fun int n = print (Int.toString n)
    fun ints ns = withSep (",", Int.toString, ns)
    fun ln s = (print s; print "\n")

  end

fun main _ = (Print.ints (1::2::3::4::5::nil); print "\n")

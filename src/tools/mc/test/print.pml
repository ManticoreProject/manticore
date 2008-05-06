signature PRINT = 
  sig
    val 'a with : string * ('a -> string) * ('a list) -> unit
    val int : int -> unit
    val ints : int list -> unit
    val ln : string -> unit
  end

structure Print (* : PRINT *) = 
  struct
    fun with (sep, mkS, xs) = print "ha" (* let
      fun w (ys, acc) =
       (case ys
         of nil => acc
          | y::ys => w (ys, acc ^ sep ^ mkS y) 
        (* end case *)) 
      val s =
       (case xs
          of nil => ""
           | x::nil => mkS x
           | x::xs => w (xs, mkS x)
         (* end case *))
      in
        print s
      end *)
    fun int n = print (itos n)
    fun ints ns = with (",", itos, ns)
    fun ln s = (print s; print "\n")
  end

fun main _ = (Print.ints (1::2::3::4::5::nil); print "\n")

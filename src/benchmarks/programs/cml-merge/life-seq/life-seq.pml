(* life-seq.pml
 *
 *)

structure LifeSeq (* : sig

    type generation

    val mkgen : (int * int) list -> generation
    val show : (string -> unit) -> generation -> unit
    val doit : unit -> unit
    val testit : unit -> unit

  end *) = struct


    val concat = String.concat
    val app = List.app
    fun cons a x = a::x
    val map = List.map
    val rev = List.rev
    fun not b = if b then false else true
    fun compose f g x = f (g x)
    val exists = List.exists
    val filter = List.filter
    val length = List.length

    fun accumulate f = let
	  fun foldf a xs = (
	        case xs
		 of nil => a
		  | (b::x) => foldf (f a b) x
	        (* end case *))
          in
	    foldf
	  end

    fun equal a b = (a = b)

    fun member x a = exists (equal a) x

    fun C f x y = f y x

    fun revonto x = accumulate (C cons) x

    fun repeat f = let fun rptf n x = if n=0 then x else rptf(n-1)(f x)
                       fun check n = if n<0 then raise Fail "repeat<0" else n
                    in compose rptf check end

    fun copy n x = repeat (cons x) n nil

    fun spaces n = concat (copy n " ")

    local 
      fun lexordset xs = (
	    case xs
	     of nil => nil
	      | (a::x) => lexordset (filter (lexless a) x) @ (a::nil) @
                             lexordset (filter (lexgreater a) x)
            (* end case *))
      and lexless(a1:int,b1:int)(a2,b2) = 
           if a2<a1 then true else if a2=a1 then b2<b1 else false
      and lexgreater pr1 pr2 = lexless pr2 pr1
      fun collect f list =
             let fun accumf sofar xs = (
		       case xs
			of nil => sofar
			 | (a::x) => accumf (revonto sofar (f a)) x
		       (* end case *))
              in accumf nil list
             end
      fun occurs3 x = 
          (* finds coords which occur exactly 3 times in coordlist x *)
          let fun f xover x3 x2 x1 xs = (
		    case xs
		     of nil => diff x3 xover
                      | (a::x) =>
		       if member xover a then f xover x3 x2 x1 x else
		       if member x3 a then f (a::xover) x3 x2 x1 x else
		       if member x2 a then f xover (a::x3) x2 x1 x else
		       if member x1 a then f xover x3 (a::x2) x1 x else
					   f xover x3 x2 (a::x1) x
	            (* end case *))
              and diff x y = filter (compose not (member y)) x
           in f nil nil nil nil x end
     in 
      datatype generation = GEN of (int*int) list

          fun alive (GEN livecoords) = livecoords
          fun mkgen coordlist = GEN (lexordset coordlist)
          fun mk_nextgen_fn neighbours gen =
              let val living = alive gen
                  val isalive = member living
                  val liveneighbours = compose length (compose (filter isalive) neighbours)
                  fun twoorthree n = n=2 orelse n=3
	          val survivors = filter (compose twoorthree liveneighbours) living
	          val newnbrlist = collect (compose (filter (compose not isalive)) neighbours) living
	          val newborn = occurs3 newnbrlist
	       in mkgen (survivors @ newborn) end

    end

    fun neighbours (i,j) = (i-1,j-1)::(i-1,j)::(i-1,j+1)::
			    (i,j-1)::(i,j+1)::
			    (i+1,j-1)::(i+1,j)::(i+1,j+1)::nil

    local val xstart = 0 val ystart = 0
          fun markafter n string = string ^ spaces n ^ "0"
          fun plotfrom (x,y) (* current position *)
                       str   (* current line being prepared -- a string *)
                       coords  (* coordinates to be plotted *)
              = (
	      case coords
	       of ((x1,y1)::more) => if x=x1
                 then (* same line so extend str and continue from y1+1 *)
                      plotfrom(x,y1+1)(markafter(y1-y)str)more
                 else (* flush current line and start a new line *)
                      str :: plotfrom(x+1,ystart)""((x1,y1)::more)
		| nil => str::nil
	      (* end case *))
           fun good (x,y) = x>=xstart andalso y>=ystart
     in  fun plot coordlist = plotfrom(xstart,ystart) "" 
                                 (filter good coordlist)
    end


    fun at(coordlist, (x:int,y:int)) = let fun move(a,b) = (a+x,b+y) 
                                      in map move coordlist end
    val rotate = map (fn (x:int,y:int) => (y,~x))

    val glider = (0,0)::(0,2)::(1,1)::(1,2)::(2,1)::nil
    val bail = (0,0)::(0,1)::(1,0)::(1,1)::nil
    fun barberpole n =
       let fun f i = if i=n then (n+n-1,n+n)::(n+n,n+n)::nil
                       else (i+i,i+i+1)::(i+i+2,i+i+1)::f(i+1)
        in (0,0)::(1,0):: f 0
       end

    val genB = mkgen(at(glider, (2,2)) @ at(bail, (2,12))
		     @ at(rotate (barberpole 4), (5,20)))

    fun nthgen (g, i) = (case i
	   of 0 => g
	    | i => nthgen (mk_nextgen_fn neighbours g, i-1)
	  (* end case *))

    val gun = mkgen
     ((2,20)::(3,19)::(3,21)::(4,18)::(4,22)::(4,23)::(4,32)::(5,7)::(5,8)::(5,18)::
      (5,22)::(5,23)::(5,29)::(5,30)::(5,31)::(5,32)::(5,36)::(6,7)::(6,8)::(6,18)::
      (6,22)::(6,23)::(6,28)::(6,29)::(6,30)::(6,31)::(6,36)::(7,19)::(7,21)::(7,28)::
      (7,31)::(7,40)::(7,41)::(8,20)::(8,28)::(8,29)::(8,30)::(8,31)::(8,40)::(8,41)::
      (9,29)::(9,30)::(9,31)::(9,32)::nil)

    fun show pr = compose (compose (app (fn s => (pr s; pr "\n"))) plot) alive

  end (* Life *)


structure Main =
  struct


    val dfltN = 100
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () =
		(* uncomment "show print" below if you wish to see the output *)
		(*show print*) (LifeSeq.nthgen(LifeSeq.gun, n))

	in
	    RunSeq.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())

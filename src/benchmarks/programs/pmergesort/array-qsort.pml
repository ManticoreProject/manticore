structure ArrayQSort =
  struct

    val sub = Rope.S.sub
    val update = Rope.S.update
    val length = Rope.S.length
    fun greater ord = 
	(case ord
	  of GREATER => true
	   | _ => false)

    fun isort (array, start, n, cmp) = let
          fun item i = sub(array,i)
          fun swap (i,j) = let 
                val tmp = sub(array,i)
                in update(array,i,sub(array,j)); update(array,j,tmp) end
          fun vecswap (i,j,n) =
	      if n = 0 then
		  ()
	      else
		  (swap(i,j);vecswap(i+1,j+1,n-1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+1)
                              else let
                                val j' = j - 1
                                in
                                  if greater(cmp(item j',item j))
                                    then (swap(j,j'); inner j')
                                    else outer(i+1)
                                end
                        in inner i end
                in
                  outer (start+1)
                end
          in insertSort (start, n); array end

    fun sortRange (array, start, n, cmp) = let
          fun item i = sub(array,i)
          fun swap (i,j) = let 
                val tmp = sub(array,i)
                in update(array,i,sub(array,j)); update(array,j,tmp) end
          fun vecswap (i,j,n) =
	      if n = 0 then
		  () 
	      else
		  (swap(i,j);vecswap(i+1,j+1,n-1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+1)
                              else let
                                val j' = j - 1
                                in
                                  if greater(cmp(item j',item j))
                                    then (swap(j,j'); inner j')
                                    else outer(i+1)
                                end
                        in inner i end
                in
                  outer (start+1)
                end

          fun med3(a,b,c) = let
		val a' = item a 
		val b' = item b 
		val c' = item c
		in
		  case (cmp(a', b'),cmp(b', c'))
		   of (LESS, LESS) => b
		    | (LESS, _) => (
			case cmp(a', c') of LESS => c | _ => a)
		    | (_, GREATER) => b
                    | _ => (case cmp(a', c') of LESS => a | _ => c)
		  (* end case *)
		end

          fun getPivot (a,n) = 
                if n <= 7 then a + n div 2
                else let
                  val p1 = a
                  val pm = a + n div 2
                  val pn = a + n - 1
                  in
                    if n <= 40 then med3(p1,pm,pn)
                    else let
                      val d = n div 8
                      val p1 = med3(p1,p1+d,p1+2*d)
                      val pm = med3(pm-d,pm,pm+d)
                      val pn = med3(pn-2*d,pn-d,pn)
                      in
                        med3(p1,pm,pn)
                      end
                  end
          
          fun quickSort arg = let
	        val (a, n) = arg
                fun bottom limit = let
                      fun loop (pa,pb) = 
                            if pb > limit then (pa,pb)
                            else case cmp(item pb,item a) of
                              GREATER => (pa,pb)
                            | LESS => loop (pa,pb+1)
                            | _ => (swap (pa,pb); loop (pa+1,pb+1))
                      in loop end
      
                fun top limit = let
                      fun loop (pc,pd) =
                            if limit > pc then (pc,pd)
                            else case cmp(item pc,item a) of
                              LESS => (pc,pd)
                            | GREATER => loop (pc-1,pd)
                            | _ => (swap (pc,pd); loop (pc-1,pd-1))
                      in loop end

                fun split (pa,pb,pc,pd) = let
                      val (pa,pb) = bottom pc (pa,pb)
                      val (pc,pd) = top pb (pc,pd)
                      in
                        if pb > pc then (pa,pb,pc,pd)
                        else (swap(pb,pc); split(pa,pb+1,pc-1,pd))
                      end

                val pm = getPivot arg
                val _ = swap(a,pm)
                val pa = a + 1
                val pc = a + (n-1)
                val (pa,pb,pc,pd) = split(pa,pa,pc,pc)
                val pn = a + n
                val r = Int.min(pa - a, pb - pa)
                val _ = vecswap(a, pb-r, r)
                val r = Int.min(pd - pc, pn - pd - 1)
                val _ = vecswap(pb, pn-r, r)
                val n' = pb - pa
                val _ = if n' > 1 then sort(a,n') else ()
                val n' = pd - pc
                val _ = if n' > 1 then sort(pn-n',n') else ()
                in () end

          and sort (x, n) = if n < 7 then insertSort (x, n)
                                     else quickSort (x, n)
          in sort (start,n) end

    fun sort cmp array = sortRange(array, 0, length array, cmp)

  end (* ArraySort *)


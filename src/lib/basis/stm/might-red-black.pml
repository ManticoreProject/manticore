structure STM = STM

type 'a tvar = 'a STM.tvar

datatype color = Red | Black | DBlack | NBlack  (*double black and negative black used for deletion*)
datatype tree = L        (*leaf*)
              | DBL      (*double black *)
              | T of color * tree tvar * int * tree tvar

fun write(t, f) = 
    let val stream = TextIO.openOut f
        val _ = TextIO.outputLine("digraph G {\n", stream)
        fun cToStr c = case c of Red => "red" | Black => "black"
        fun lp t i = 
            case STM.get t
                of L => i
                 | DBL => i
                 | T(c, l, v, r) => 
                    case (STM.get l, STM.get r)
                        of (T(c1,l1,v1,r1),T(c2,l2,v2,r2)) =>
                            let val _ = TextIO.outputLine(Int.toString v ^ " -> " ^ Int.toString v1 ^ ";\n", stream)
                                val _ = TextIO.outputLine(Int.toString v ^ " -> " ^ Int.toString v2 ^ ";\n", stream)
                                val _ = TextIO.outputLine(Int.toString v ^ " [color = " ^ cToStr c ^ "];\n", stream)
                            in lp r (lp l i) end
                         |(_, T(c',l',v',r')) => 
                            let val _ = TextIO.outputLine(Int.toString v ^ " -> " ^ Int.toString v' ^ ";\n", stream)
                            val _ = TextIO.outputLine(Int.toString v ^ " [color = " ^ cToStr c ^ "];\n", stream)
                            val n = lp r i
                            val _ = TextIO.outputLine(Int.toString v ^ " -> " ^ "L" ^ Int.toString n ^ ";\n", stream)
                            in n+1 end
                         |(T(c',l',v',r'), _) => 
                            let val _ = TextIO.outputLine(Int.toString v ^ " -> " ^ Int.toString v' ^ ";\n", stream)
                            val _ = TextIO.outputLine(Int.toString v ^ " [color = " ^ cToStr c ^ "];\n", stream)
                            val n = lp r i
                            val _ = TextIO.outputLine(Int.toString v ^ " -> " ^ "L" ^ Int.toString n ^ ";\n", stream)
                            in n+1 end
                         |_ => 
                            let val _ = TextIO.outputLine(Int.toString v ^ " [color = " ^ cToStr c ^ "];\n", stream)
                                val _ = TextIO.outputLine(Int.toString v ^ " -> " ^ "L" ^ Int.toString i ^ ";\n", stream)
                                val _ = TextIO.outputLine(Int.toString v ^ " -> " ^ "L" ^ Int.toString (i+1) ^ ";\n", stream)
                            in i+2 end
        val _ = lp t 0
        val _ = TextIO.outputLine("}\n", stream)
        val _ = TextIO.closeOut stream          
    in () end




fun intComp(x:int,y:int) : order = if x < y then LESS else if x > y then GREATER else EQUAL

fun redden t = 
    case STM.get t
        of T(Red, a, x, b) => ()
         | T(Black, a, x, b) => STM.put(t, T(Red, a, x, b))

fun blacken' t = 
    case STM.get t
        of T(Red, a, x, b) => STM.put(t, T(Black, a, x, b))
         | T(Black, a, x, b) => ()          

fun blacken t = 
    case STM.get t
        of T(Black,a,x,b) => ()
         | T(DBlack, a, x, b) => STM.put(t, T(Black, a, x, b))
         | L => ()
         | DBL => STM.put(t, L)

fun isBB t = 
    case STM.get t
        of DBL => true
         | T(DBlack,_,_,_) => true
         | _ => false

fun blacker c = 
    case c 
        of NBlack => Red 
         | Red => Black
         | Black => DBlack
         | DBlack => raise Fail "Too black"

fun redder c = 
    case c 
        of Red => NBlack
         | Black => Red
         | DBlack => Black
         | NBlack => raise Fail "Not black enough"

fun blacker' t =
    case STM.get t
        of L => STM.put(t, DBL)
         | T(c,l,x,r) => STM.put(t, T(blacker c, l, x, r))
         | DBL => raise Fail "Too black\n"

fun redder' t = 
    case STM.get t
        of DBL => STM.put(t, L)
         | T(c,l,x,r) => STM.put(t, T(redder c, l, x, r))
         | L => raise Fail "Trying to create red leaf\n"      

fun member (x:int) (t:tree tvar) (compare: (int*int) -> order) : bool = 
    let fun lp t = 
            case STM.get t 
                of L => false
                 | T(c, l, v, r) =>
                    (case compare(x, v)
                        of LESS => lp l
                         | GREATER => lp r
                         | EQUAL => true)
                 | DBL => raise Fail "found double black leaf in member\n"
    in STM.atomic(fn () => lp t) end

fun balance tv = 
    case STM.get tv
        of T(Red,t1,k,t2) => false
         | T(Black,t1,k,t2) =>
            if (case STM.get t1
                of T(Red,l',y,r') =>
                    (case (STM.get l', STM.get r')
                        of (T(Red,a,x,b), _) => 
                            let val _ = STM.put(l', T(Black,a,x,b))
                                val r = STM.new(T(Black, r', k, t2))
                                val _ = STM.put(tv, T(Red, l', y, r))
                            in true end
                         | (_,T(Red,b,z,c)) => 
                            let val _ = STM.put(r', T(Black, l', y, b))
                                val r = STM.new(T(Black,c,k,t2))
                                val _ = STM.put(tv, T(Red,r',z,r))
                            in true end
                         | _ => false)
                  | _ => false)
             then true
             else (case STM.get t2 
                    of T(Red,l',y,r') =>
                        (case (STM.get l', STM.get r')
                            of (T(Red,b,z,c),_) =>
                                let val _ = STM.put(l', T(Black,c,y,r'))
                                    val l = STM.new(T(Black,t1,k,b))
                                    val _ = STM.put(tv, T(Red,l,z,l'))
                                in true end
                            | (_,T(Red,c,z,d)) =>
                                let val _ = STM.put(r', T(Black,c,z,d))
                                    val l = STM.new(T(Black,t1,k,l'))
                                    val _ = STM.put(tv, T(Red,l,y,r'))
                                in true end
                            | _ => false)
                    | _ => false)
          | _ => false          
                                               
fun makeBlack t = 
    case STM.get t
        of L => ()
         | T(c, l, v, r) => STM.put(t, T(Black, l, v, r))
         | DBL => raise Fail "Found double black leaf in make black\n"

exception NoChange
fun insert (x:int) (t:tree tvar) (compare : int*int -> order) : unit =
    let fun lp t = 
            case STM.get t
                of L => STM.put(t, T(Red, STM.new L, x, STM.new L))
                 | T(c,l,v,r) =>
                    (case compare(x, v)
                        of LESS => (lp l; balance t; ())
                         | GREATER => (lp r; balance t; ())
                         | EQUAL => ())
                 | DBL => raise Fail "found double black leaf in insert\n"
    in STM.atomic(fn () => (lp t; makeBlack t)) end

fun isBlack t = 
    case STM.get t
        of T(Black,_,_,_) => true
         | L => true
         | _ => false

fun delBalance tv = 
    if balance tv
    then true
    else case STM.get tv
            of T(Red,t1,k,t2) => false
             | T(DBlack,t1,k,t2) =>
                if (case STM.get t1
                    of T(Red,l',y,r') =>
                        (case (STM.get l', STM.get r')
                            of (T(Red,a,x,b), _) => 
                                let val _ = STM.put(l', T(Black,a,x,b))
                                    val r = STM.new(T(Black, r', k, t2))
                                    val _ = STM.put(tv, T(Black, l', y, r))
                                in true end
                             | (_,T(Red,b,z,c)) => 
                                let val _ = STM.put(r', T(Black, l', y, b))
                                    val r = STM.new(T(Black,c,k,t2))
                                    val _ = STM.put(tv, T(Black,r',z,r))
                                in true end
                             | _ => false)
                     | T(NBlack,l',y,r') =>  (*T(DBlack, T(NBlack,l' as T(Black,_,_,_),y,T(Black,b,y,c)), k, t2)*)
                        (case (STM.get l', STM.get r')
                            of (T(Black,ll,vv,rr), T(Black,b,z,c)) =>
                                let val _ = STM.put(l', T(Red, ll, vv, rr))
                                    val _ = STM.put(t1, T(Black,l',y, b))
                                    val newR = STM.new(T(Black, c, k, t2))
                                    val _ = STM.put(tv, T(Black, t1, z, newR))
                                in delBalance t1 end
                             | _ => false)
                      | _ => false)
                 then true
                 else (case STM.get t2 
                        of T(Red,l',y,r') =>
                            (case (STM.get l', STM.get r')
                                of (T(Red,b,z,c),_) =>
                                    let val _ = STM.put(l', T(Black,c,y,r'))
                                        val l = STM.new(T(Black,t1,k,b))
                                        val _ = STM.put(tv, T(Black,l,z,l'))
                                    in true end
                                | (_,T(Red,c,z,d)) =>
                                    let val _ = STM.put(r', T(Black,c,z,d))
                                        val l = STM.new(T(Black,t1,k,l'))
                                        val _ = STM.put(tv, T(Black,l,y,r'))
                                    in true end
                                | _ => false)
                        | T(NBlack,l',y,r') => (*T(DBlack,t1,k,T(NBlack,l',y,r'))*)
                                (case (STM.get l', STM.get r') 
                                    of (T(Black,b,z,c), T(Black,_,_,_)) => (*T(Dblack, t1, k, T(NBlack, T(Black, b, z, c), y, r' as T(Black, _, _, _))) *)
                                        let val _ = redden r'
                                            val _ = STM.put(t2, T(Black, c, y, r'))
                                            val newL = STM.new(T(Black, t1, k, b))
                                            val _ = STM.put(tv, T(Black, newL, z, t2))
                                        in delBalance t2 end
                                     | _ => false)
                        | _ => false)
             | _ => false            

fun bubble t = 
    case STM.get t
        of T(c,l,x,r) =>
            if isBB l orelse isBB r
            then (STM.put(t, T(blacker c, l, x, r)); redder' l; redder' r; delBalance t; ())
            else (delBalance t; ())
         | _ => ()

(*Precondition: t has only one child. *)
fun remove' t : unit = 
    case STM.get t
        of T(Red,l,v,r) => STM.put(t, L)    (*l anv v are necessarily L*)
         | T(Black,l,v,r) =>    
            (case STM.get l
                of T(Red,a,x,b) => STM.put(t, T(Black,a,x,b))
                 | _ => case STM.get r
                            of T(Red, a, x, b) => STM.put(t, T(Black, a, x, b))
                             | _ => STM.put(t, DBL))
         | _ => raise Fail "Impossible: removePrime"

fun remove (x:int) (t:tree tvar) (compare:int*int-> order) = 
    let fun removeMax t = 
            case STM.get t
                of T(c,l,v,r) => 
                    (case STM.get r
                        of L => (remove' t; v)
                         | _ => let val v = removeMax r
                                    val _ = bubble t
                                in v end)
                 | _ => raise Fail "Impossible: remove"
        fun lp t = 
            case STM.get t
                of L => ()
                 | T(c,l,v,r) => 
                    (case compare(x, v)
                        of GREATER => (lp r; bubble t)
                         | LESS => (lp l; bubble t)
                         | EQUAL => 
                            (case STM.get l
                                of L => remove' t
                                 | _ => (case STM.get r
                                            of L => remove' t
                                             | _ => (STM.put(t, T(c, l, removeMax l, r)); bubble t))))
                 | DBL => raise Fail "found double black leaf in remove:lp\n"                            
    in STM.atomic(fn _ => (lp t ; makeBlack t)); ()
    end

(*Verify red-black tree properties*)         
fun chkOrder t = 
    let fun lp(t, lower, upper) = 
            case STM.get t
                of L => true
                 | T(c,l,v,r) =>   
                    let val b1 = lp(l, lower, SOME v)
                        val b2 = lp(r, SOME v, upper)
                    in case (lower, upper)
                        of (NONE, NONE) => true
                         | (NONE, SOME u) => v < u
                         | (SOME l, NONE) => v > l
                         | (SOME l, SOME u) => v > l andalso v < u
                    end
                 | DBL => raise Fail "found double black leaf in chkOrder\n"
    in if lp(t, NONE, NONE) 
       then print "Red black tree order is correct\n" 
       else print "Red black tree order is incorrect\n"
    end

datatype expected = MustBeBlack | Any

fun chkBlackPaths t = 
    let fun lp(t, exp, d) =
            case (exp, STM.get t)
                of (Any, T(Red, l, v, r)) =>
                    let val n : int = lp(l, MustBeBlack, d+1) 
                        val n' : int = lp(r, MustBeBlack, d+1) 
                        val _ = if n <> n' then raise Fail "Incorrect number of nodes (red)\n" else ()
                    in n end       
                 | (MustBeBlack, T(Red, _, _, _)) => (raise Fail ("Incorrect: found red, when expected black at depth " ^ Int.toString d ^ "\n"))
                 | (_, T(Black, l, v, r)) =>
                    let val n : int = lp(l, Any, d+1)
                        val n' : int = lp(r, Any, d+1)
                        val _ = if n <> n' then raise Fail "Incorrect number of nodes (black)\n" else ()
                    in n+1 end                 
                 | (_, L) => 0
                 | _ => raise Fail "Impossible: chkPlackPaths\n"
   in lp(t, Any, 0); print "Red-Black property holds\n" end              

val ITERS = 40000
val THREADS = 4
val MAXVAL = 10000000

fun ignore _ = ()

val READS = 2
val WRITES = 2
val DELETES = 2

fun threadLoop t i = 
    if i = 0
    then ()
    else let val randNum = Rand.inRangeInt(0, MAXVAL)
             val prob = Rand.inRangeInt(0, READS+WRITES+DELETES)
             val _ = if prob < READS
                     then ignore(member randNum t intComp)
                     else if prob < READS + WRITES
                          then ignore(insert randNum t intComp)
                          else ignore(remove randNum t intComp)
         in threadLoop t (i-1) end
         
fun start t i =
    if i = 0
    then nil
    else let val ch = PrimChan.new()
             val _ = Threads.spawnOn(i-1, fn _ => (threadLoop t ITERS; PrimChan.send(ch, i)))
         in ch::start t (i-1) end

fun join chs = 
    case chs
        of ch::chs' => (PrimChan.recv ch; join chs')
         | nil => ()

val t = STM.new L

fun initialize n = 
    if n = 0
    then ()
    else let val randNum = Rand.inRangeInt(0, MAXVAL)
             val _ = insert randNum t intComp
         in initialize (n-1) end

val _ = initialize 1000
val startTime = Time.now()
val _ = join(start t THREADS)
val endTime = Time.now()
val _ = STM.printStats()
val _ = print ("Total was: " ^ Time.toString (endTime - startTime) ^ " seconds\n")

val _ = chkOrder t
val _ = chkBlackPaths t handle Fail s => print s






















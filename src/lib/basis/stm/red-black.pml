structure STM = STM

type 'a tvar = 'a STM.tvar

datatype color = Red | Black | DBlack  (*double black used for deletion*)
datatype tree = L        (*leaf*)
              | DBL      (*double black *)
              | T of color * tree tvar * int * tree tvar

fun intComp(x:int,y:int) : order = if x < y then LESS else if x > y then GREATER else EQUAL

fun member (x:int) (t:tree tvar) (compare: (int*int) -> order) : bool = 
    let fun lp t = 
            case STM.get t 
                of L => false
                 | T(c, l, v, r) =>
                    (case compare(x, v)
                        of LESS => lp l
                         | GREATER => lp r
                         | EQUAL => true)
    in STM.atomic(fn () => lp t) end

fun balance tv = 
    case STM.get tv
        of T(Red,t1,k,t2) => ()
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
             then ()
             else (case STM.get t2 
                    of T(Red,l',y,r') =>
                        (case (STM.get l', STM.get r')
                            of (T(Red,b,z,c),_) =>
                                let val _ = STM.put(l', T(Black,c,y,r'))
                                    val l = STM.new(T(Black,t1,k,b))
                                    val _ = STM.put(tv, T(Red,l,z,l'))
                                in () end
                            | (_,T(Red,c,z,d)) =>
                                let val _ = STM.put(r', T(Black,c,z,d))
                                    val l = STM.new(T(Black,t1,k,l'))
                                    val _ = STM.put(tv, T(Red,l,y,r'))
                                in () end
                            | _ => ())
                    | _ => ())
                    
fun makeBlack t = 
    case STM.get t
        of L => ()
         | T(c, l, v, r) => STM.put(t, T(Black, l, v, r))

exception NoChange
fun insert (x:int) (t:tree tvar) (compare : int*int -> order) : unit =
    let fun lp t = 
            case STM.get t
                of L => STM.put(t, T(Red, STM.new L, x, STM.new L))
                 | T(c,l,v,r) =>
                    case compare(x, v)
                        of LESS => (lp l; balance t)
                         | GREATER => (lp r; balance t)
                         | EQUAL => ()
    in STM.atomic(fn () => (lp t; makeBlack t)) end

fun remove (x:int) (t:tree tvar) (compare:int*int-> order) = 
    let (*returns true if the result needs to be fixed up as well*)                        
        fun fixup (t : tree tvar) : bool = 
            case STM.get t
                of L => false
                 | T(c,l,v,r) => 
                    (case (STM.get l, STM.get r)
                        of (T(DBlack,l1,v1,r1),T(Red,l2,v2,r2)) =>  (*case 1a*)
                            let val _ = STM.put(r, STM.get r2)
                                val lNew = STM.new (T(Red,l,v,l2))
                                val _ = STM.put(t, T(c,lNew,v2,r))
                            in fixup lNew end  (*this should always return false*)
                         | (T(DBlack,l1,v1,r1),T(Black,l2,v2,r2)) =>
                            (case (STM.get l2, STM.get r2)
                                of (T(Black,_,_,_),T(Black,_,_,_)) =>   (*case 2a*)
                                    let val _ = STM.put(l,T(Black,l1,v1,r1))
                                        val _ = STM.put(r, T(Red,l2,v2,r2))
                                    in case c
                                        of Red => (STM.put(t,T(Black,l,v,r)); false)
                                         | Black => (STM.put(t,T(DBlack,l,v,r)); true)
                                    end     
                                 | (T(Red,l21,v21,r21),T(Black,l22,v22,r22)) => (*case 3a*)
                                    let val r' = STM.new(T(Red,r21,v2,r2))
                                        val _ = STM.put(r, T(Black,l21,v21,r'))
                                        val _ = STM.put(t, T(c,l,v,r))
                                    in fixup t end  
                                 | (_,T(Red,l22,v22,r22)) =>                        (*case 4a*)
                                    let val l' = STM.new(T(Black,l1,v,l2))
                                        val _ = STM.put(r2, T(Black,l22,v22,r22))
                                        val _ = STM.put(t, T(c,l',v2,r2))
                                    in false end
                                 | _ => false
                                  )
                         | (T(Red,l1,v1,r1),T(DBlack,l2,v2,r2)) =>  (*case 1b*)
                            let val _ = STM.put(l, STM.get l2)
                                val r' = STM.new (T(Red,r,v,r2))
                                val _ = STM.put(t, T(c,r',v2,l))
                            in fixup r' end  (*this should always return false*)                                  
                        | (T(Black,l1,v1,r1),T(DBlack,l2,v2,r2)) =>  
                            (case (STM.get l1, STM.get r1)
                                of (T(Black,_,_,_),T(Black,_,_,_)) => (*case 2b*)
                                    let val _ = STM.put(l,T(Red,l1,v1,r1))
                                        val _ = STM.put(r, T(Black,l2,v2,r2))
                                    in case c
                                        of Red => (STM.put(t,T(Black,l,v,r)); false)
                                         | Black => (STM.put(t,T(DBlack,l,v,r)); true)
                                    end     
                                 | (T(Red,l21,v21,r21),T(Black,l22,v22,r22)) => (*case 3b*)
                                    let val l' = STM.new(T(Red,r21,v2,r2))
                                        val _ = STM.put(l, T(Black,l21,v21,l'))
                                        val _ = STM.put(t, T(c,l,v,r))
                                    in fixup t end  
                                 | (_,T(Red,l22,v22,r22)) =>                  (*case 4b*)
                                    let val r' = STM.new(T(Black,l1,v,l2))
                                        val _ = STM.put(r2, T(Black,l22,v22,r22))
                                        val _ = STM.put(t, T(c,r',v2,r2))
                                    in false end 
                                | _ => false)
                       | _ => false    )      
                    
        fun removeLeftmost (t : tree tvar) : (bool * int) = 
            case STM.get t
                of T(c,l,v,r) =>
                    (case (STM.get l, STM.get r)
                        of (L, L) => (STM.put(t, L); (false, v))
                         | (L, T(Red,l',v',r')) => (STM.put(t, T(Red,l',v',r')); (false, v))
                         | (L, T(Black,l',v',r')) => (STM.put(t, T(DBlack,l',v',r')); (true, v))
                         | (T(c',l',v',r'), _) => 
                            let val (b, v) = removeLeftmost l 
                            in if b then (fixup t, v) else (false, v) end)
                 | _ => (print "IMPOSSIBLE: removeLeftmost\n"; raise Fail "impossible")          
        fun lp (t: tree tvar) : bool = 
            case STM.get t
                of L => false
                 | T(c,l,v,r) =>
                    (case compare(x, v)
                        of GREATER => (if lp r then fixup t else false)
                         | LESS => (if lp l then fixup t else false)
                         | EQUAL => 
                            (case (STM.get l, STM.get r)
                                of (L, L) => (STM.put(t, L); false)
                                 | (L, T(Red,l',v',r')) => (STM.put(t, T(Black,l',v',r')); false)
                                 | (L, T(Black,l',v',r')) => (STM.put(t, T(DBlack,l',v',r')); true)
                                 | (T(Red,l',v',r'), L) => (STM.put(t, T(Black,l',v',r')); false)
                                 | (T(Black,l',v',r'),L) => (STM.put(t, T(DBlack,l',v',r')); true)
                                 | (T(c1,l1,v1,r1),T(c2,l2,v2,r2)) =>
                                    let val (b, nextV) = removeLeftmost r
                                        val _ = STM.put(t, T(c,l,nextV,r))
                                    in if b then fixup t else false end))
    in STM.atomic(fn () => (lp t; makeBlack t)) end                                   

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
                 | (MustBeBlack, T(Red, _, _, _)) => (raise Fail ("Incorrect: found red, when expected black at depth " ^ Int.toString d))
                 | (_, T(Black, l, v, r)) =>
                    let val n : int = lp(l, Any, d+1)
                        val n' : int = lp(r, Any, d+1)
                        val _ = if n <> n' then raise Fail "Incorrect number of nodes (black)\n" else ()
                    in n end                 
                 | (_, L) => 0
   in lp(t, Any, 0); print "Red-Black property holds\n" end              

val t : tree tvar = STM.new L

fun printTree t = 
    case STM.get t
        of L => "L"
         | T(Red,l,v,r) =>
            ("T(Red, " ^ printTree l ^ ", " ^ Int.toString v ^ ", " ^ printTree r ^ ")")
        | T(Black,l,v,r) =>
            ("T(Black, " ^ printTree l ^ ", " ^ Int.toString v ^ ", " ^ printTree r ^ ")")
            
fun addNums i t =
    if i = 0
    then nil
    else let val randNum = Rand.inRangeInt(0, 10000000)
             val _ = insert randNum t intComp
             val coin = Rand.inRangeInt(0, 2)
         in if coin = 0 then randNum::addNums (i-1) t else addNums (i-1) t end


fun removeNums ns t =  
    case ns
        of nil => ()
         | n::ns => 
            let val _ = remove n t intComp
                val _ = if member n t intComp then print "Was not removed\n" else ()
            in removeNums ns t end
         
val toBeRemoved = addNums 1000 t

val _ = chkOrder t
val _ = chkBlackPaths t handle Fail s => print s

fun height t = 
    case STM.get t 
        of L => 0
         | T(_,l,_,r) => 1 + Int.max(height l, height r)

val _ = print ("Height of tree is " ^ Int.toString (height t) ^ "\n")     

val _ = print ("Removing " ^ Int.toString(List.length toBeRemoved) ^ " nodes\n")

val _ = removeNums toBeRemoved t

val _ = chkOrder t
val _ = chkBlackPaths t handle Fail s => print s

val _ = print ("Height of tree is " ^ Int.toString (height t) ^ "\n")       


fun mkL() = STM.new L
fun mkSingle(c, v) = STM.new(T(c, mkL(), v, mkL()))
fun mkT(c,l,v,r) = STM.new(T(c,l,v,r))




























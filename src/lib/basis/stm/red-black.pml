
type 'a tvar = 'a STM.tvar

datatype color = Red | Black | DBlack  (*double black used for deletion*)
datatype 'a tree = E 
                 | T of color * 'a tree tvar * 'a * 'a tree tvar

fun intComp(x:int,y:int) : order = if x < y then LESS else if x > y then GREATER else EQUAL

fun member (x:'a) (t:'a tree tvar) (compare: ('a * 'a) -> order) : bool = 
    case STM.get t 
        of E => false
         | T(c, l, v, r) =>
            (case compare(x, v)
                of LESS => member x l compare
                 | GREATER => member x r compare
                 | EQUAL => true)

fun balance tv = 
    case STM.get tv
        of T(Red,t1,k,t2) => ()
         | T(_,t1,k,t2) =>
            case STM.get t1
                of T(Red,l',y,r') =>
                    (case (STM.get l', STM.get r')
                        of (T(Red,a,x,b), _) =>  (*rotate right*)
                            let val _ = STM.put(l', T(Black,a,x,b))
                                val r = STM.new(T(Black, r', k, t2))
                                val _ = STM.put(tv, T(Red, l', y, r))
                                val _ = print "Rotating\n"
                            in () end
                         | (_,T(Red,b,z,c)) => 
                            let val _ = STM.put(r', T(Black, l', z, b))
                                val r = STM.new(T(Black,c,k,t2))
                                val _ = STM.put(tv, T(Red,r',y,r))
                                val _ = print "Rotating\n"
                            in () end
                         | _ => 
                            case STM.get t2 
                                of T(Red,l',y,r') =>
                                    case (STM.get l', STM.get r') 
                                        of (T(Red,b,z,c),_) =>
                                            let val _ = STM.put(l', T(Black,c,z,r'))
                                                val l = STM.new(T(Black,t1,k,b))
                                                val _ = STM.put(tv, T(Red,l,y,l'))
                                                val _ = print "Rotating\n"
                                            in () end
                                        | (_,T(Red,c,z,d)) =>
                                            let val _ = STM.put(r', T(Black,c,z,d))
                                                val l = STM.new(T(Black,t1,k,l'))
                                                val _ = STM.put(tv, T(Red,l,y,r'))
                                                val _ = print "Rotating\n"
                                            in () end
                                        | _ => ())
                 |_ => ()                         

fun makeBlack t = 
    case STM.get t
        of E => ()
         | T(c, l, v, r) => STM.put(t, T(Black, l, v, r))

fun insert (x:'a) (t:'a tree tvar) (compare : 'a*'a -> order) =
    let fun lp t = 
            case STM.get t
                of E => STM.put(t, T(Red, STM.new E, x, STM.new E))
                 | T(c,l,v,r) =>
                    case compare(x, v)
                        of LESS => (lp l; balance t)
                         | GREATER => (lp r; balance t)
                         | EQUAL => ()
    in lp t; makeBlack t end
         
val _ = print "test\n"
val t = STM.new (T(Red, STM.new E, 12, STM.new E))
val _ = print "test\n"
val _ = STM.atomic (fn _ => balance t)
val _ = print "test\n"
val _ = STM.atomic (fn _ => makeBlack t)
val _ = STM.atomic (fn _ => insert 12  t intComp)

val _ = if STM.atomic(fn _ => member 12 t intComp) then print "Correct\n" else print "Incorrect\n"

val t = STM.new E

fun addNums i t =
    if i = 0
    then ()
    else let val randNum = Rand.inRangeInt(0, 1000)
             val _ = STM.atomic(fn _ => insert randNum t intComp)
         in addNums (i-1) t end
         
val _ = print "test\n"         
val _ = addNums 10 t


fun chkOrder t = 
    let fun lp(t, lower, upper) = 
            case STM.get t
                of E => true
                 | T(c,l,v,r) => 
                    let val b1 = lp(l, lower, SOME v)
                        val b2 = lp(r, SOME v, upper)
                    in case (lower, upper)
                        of (NONE, NONE) => true
                         | (NONE, SOME u) => v < u
                         | (SOME l, NONE) => v > l
                         | (SOME l, SOME u) => v > l andalso v < u
                    end
    in lp(t, NONE, NONE) end
    
fun chkBlackPaths t = 
    let fun lp(t, c) =
            case (c, STM.get t)
                of (false, T(Red, l, v, r)) =>
                    let val n : int = lp(l, true)
                        val n' : int = lp(r, true)
                        val _ = if n <> n' then (print "INCORRECT\n"; raise Fail "Incorrect\n") else ()
                    in n end       
                 | (true, T(Red, _, _, _)) => (print "INCORRECT\n"; raise Fail "Incorrect\n")        
                 | (_, T(Black, l, v, r)) =>
                    let val n : int = lp(l, false)
                        val n' : int = lp(r, false)
                        val _ = if n <> n' then (print "INCORRECT\n"; raise Fail "Incorrect\n") else ()
                    in n end                 
                 | (_, E) => 0
   in lp(t, true); print "Red-Black property holds\n" end              

(*
fun mkE() = STM.new E                 
val t = STM.new (T(Black, STM.new (T(Red,STM.new (T(Black, mkE(), 1, mkE())), 2, STM.new(T(Black, STM.new (T(Red, mkE(), 5, mkE())), 7, STM.new (T(Red, mkE(), 8, mkE())))))), 11, STM.new(T(Black, mkE(), 14, STM.new(T(Red, mkE(), 15, mkE()))))))
val _ = chkBlackPaths t

val _ = insert 4 t intComp
val _ = chkBlackPaths t
*)


val _ = if chkOrder t then print "Order is correct\n" else print "Order is incorrect\n"
val _ = chkBlackPaths t 
val _ = print "Red black property is correct\n" 


(*
fun balance rb t1 k t2 = 
    case rb
        of Red => T(Red, t1, k, t2)
         | Black => 
             case t1 
                of T(Red, T(Red, a, x, b), y, c) => T(Red,T(Black, a, x, b),y,T(Black, c, k, t2))
                 | T(Red,a,x,T(Red,b,y,c)) => T(Red,T(Black,a,x,b),y,T(Black,c,k,t2))
                 | a => 
                    case t2
                        of T(Red,T(Red,b,y,c),z,d) => T(Red,T(Black,t1,k,b),y,T(Black,c,z,d))
                         | T(Red,b,y,T(Red,c,z,d)) => T(Red,T(Black,t1,k,b),y,T(Black,c,z,d))
                         | _ => T(Black, t1, k, t2)
                        
fun makeBlack t =
    case t 
        of E => E
         | T(_, l, v, r) => T(Black,l,v,r)

fun ins (x:'a) (t:'a tree) (compare : 'a * 'a -> order) = 
    let fun lp t = 
            case t 
                of E => T(Red, E, x, E)
                 | T(c,l,v,r) => 
                    case compare(x, v)
                        of LESS => balance c (lp l) v r
                         | GREATER => balance c l v (lp r)
                         | EQUAL => t
    in makeBlack (lp t) 
    end

fun delete (x:'a) (t:'a tree) (compare : 'a * 'a -> order) = 
    let fun getLeast t = 
            case t
                of T(c, E, v, r) => (c, v, r)
                 | T(c, l, v, r) => 
                    let (c',v',r') = getLeast l
                    in (c',v', T(c, l, v, r')) end
        fun lp t = 
            case t
                of E => E
                 | T(c,l,v,r) =>
                    case compare(x, v)
                        of LESS => balance c (lp t) v r         (*delete in left*)
                         | GREATER => balance c l v (lp r)      (*delete in right*)
                         | Equal =>                             (*delete this node*)
                            case (c, l, r)
                                of (c, E, E) => E                 
                                 | (Red, E, r') => r'    
                                 | (Black, E, T(Red,l',v',r')) => T(Black,l',v',r')
                                 | (Black, E, T(Black,l',v',r')) => T(DBlack,l',v',r')         
                                 | (Red, l', E) => l'  
                                 | (Black, T(Red,l',v',r'), E) => T(Black,l',v',r')            
                                 | (Black, T(Black,l',v',r'), E) => T(DBlack,l',v',r')     

val t : int tree = makeBlack(balance Red E 1 E)
val b:bool = member 1 t intComp
val t = ins 12 t intComp
*)






(* red-black.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Concurrent red black tree based on Matt Might's delete algorithm:
        -http://matt.might.net/articles/red-black-delete/
        -https://github.com/sweirich/dth/tree/master/examples/red-black
 *)
			
structure RBTree = 
struct
    type 'a tvar = 'a PartialSTM.tvar

    fun getArg f args = 
        case args 
            of arg::arg'::args => 
                if String.same(f, arg) then SOME arg'
                else getArg f (arg'::args)
             |_ => NONE

    val args = CommandLine.arguments ()

    val (get,put,atomic,new,printStats, unsafeGet) = (STM.get,STM.put,STM.atomic,STM.new,STM.printStats,STM.unsafeGet)

    datatype color = Red | Black | DBlack | NBlack  (*double black and negative black used for deletion*)
    datatype 'a tree = L        (*leaf*)
                  | DBL      (*double black *)
                  | T of color * 'a tree tvar * 'a * 'a tree tvar

    fun newTree() = new L
			
    fun redden t = 
	case get t 
	 of T(Red,a,x,b) => ()
	  | T(Black, a, x, b) => put(t, T(Red, a, x, b))

    fun isBB t = 
        case get t
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
        case get t
            of L => put(t, DBL)
             | T(c,l,x,r) => put(t, T(blacker c, l, x, r))
             | DBL => raise Fail "Too black\n"

    fun redder' t = 
        case get t
            of DBL => put(t, L)
             | T(c,l,x,r) => put(t, T(redder c, l, x, r))
             | L => raise Fail "Trying to create red leaf\n"      

    fun isEmpty t = case STM.unsafeGet t 
		     of L => true
		      | _  => false

    fun member compare x t = 
        let fun lp t = 
                case get t 
                    of L => false
                     | T(c, l, v, r) =>
                        (case compare(x, v)
                            of LESS => lp l
                             | GREATER => lp r
                             | EQUAL => true)
                     | DBL => raise Fail "found double black leaf in member\n"
        in atomic(fn () => lp t) end

    (*This is nasty, pattern matching for STM dereference would be awesome*)
    fun balance tv = 
        case get tv
           of T(Red,t1,k,t2) => ()
            | T(Black,t1,k,t2) =>
                if case get t1
                   of T(Red,l',y,r') =>
                        (case (get l', get r')
                           of (T(Red,a,x,b), _) => 
                                let val _ = put(l', T(Black,a,x,b))
                                    val r = new(T(Black, r', k, t2))
                                    val _ = put(tv, T(Red, l', y, r)) 
                                in true end
                            | (_,T(Red,b,z,c)) => 
                                let val _ = put(r', T(Black, l', y, b))
                                    val r = new(T(Black,c,k,t2))
                                    val _ = put(tv, T(Red,r',z,r)) 
                                in true end
                            | _ => false)
                    | _ => false
                then ()
                else 
                    (case get t2 
                       of T(Red,l',y,r') =>
                            (case (get l', get r')
                               of (T(Red,b,z,c),_) =>
                                    let val _ = put(l', T(Black,c,y,r'))
                                        val l = new(T(Black,t1,k,b))
                                    in put(tv, T(Red,l,z,l')) end
                                | (_,T(Red,c,z,d)) =>
                                    let val _ = put(r', T(Black,c,z,d))
                                        val l = new(T(Black,t1,k,l'))
                                    in put(tv, T(Red,l,y,r')) end
                                | _ => ())
                        | _ => ())
            | T(DBlack,t1,k,t2) =>
                (if (case get t1
                       of T(Red,l',y,r') =>
                            (case (get l', get r')
                               of (T(Red,a,x,b), _) => 
                                    let val _ = put(l', T(Black,a,x,b))
                                        val r = new(T(Black, r', k, t2))
                                        val _ = put(tv, T(Black, l', y, r)) 
                                    in true end
                                | (_,T(Red,b,z,c)) => 
                                    let val _ = put(r', T(Black, l', y, b))
                                        val r = new(T(Black,c,k,t2))
                                        val _ = put(tv, T(Black,r',z,r)) 
                                    in true end
                                | _ => false)
                        | T(NBlack,l',y,r') =>  (*T(DBlack, T(NBlack,l' as T(Black,_,_,_),y,T(Black,b,y,c)), k, t2)*)
                            (case (get l', get r')
                               of (T(Black,ll,vv,rr), T(Black,b,z,c)) =>
                                    let val _ = put(l', T(Red, ll, vv, rr))
                                        val _ = put(t1, T(Black,l',y, b))
                                        val newR = new(T(Black, c, k, t2))
                                        val _ = put(tv, T(Black, t1, z, newR))
                                        val _ = balance t1
                                    in true end
                                | _ => false)
                        | _ => false)
                then ()
                else 
                    case get t2 
                       of T(Red,l',y,r') =>
                            (case (get l', get r')
                               of (T(Red,b,z,c),_) =>
                                    let val _ = put(l', T(Black,c,y,r'))
                                        val l = new(T(Black,t1,k,b))
                                    in put(tv, T(Black,l,z,l')) end
                                | (_,T(Red,c,z,d)) =>
                                    let val _ = put(r', T(Black,c,z,d))
                                        val l = new(T(Black,t1,k,l'))
                                    in put(tv, T(Black,l,y,r')) end
                                | _ => ())
                        | T(NBlack,l',y,r') => 
                            (case (get l', get r') 
                               of (T(Black,b,z,c), T(Black,_,_,_)) => (*T(Dblack, t1, k, T(NBlack, T(Black, b, z, c), y, r' as T(Black, _, _, _))) *)
                                    let val _ = redden r'
                                        val _ = put(t2, T(Black, c, y, r'))
                                        val newL = new(T(Black, t1, k, b))
                                        val _ = put(tv, T(Black, newL, z, t2))
                                    in balance t2 end
                                | _ => ())
                        | _ => ())
            | _ => () 

    fun makeBlack t = 
        case get t
            of L => ()
             | T(Black,l,v,r) => ()
             | T(c, l, v, r) => put(t, T(Black, l, v, r))
             | DBL => put(t, L)  (*if we remove from a tree with a single black node, we could end up with a double black leaf*)

    fun insert compare x t =
        let fun lp t = 
                case get t
                    of L => put(t, T(Red, new L, x, new L))
                     | T(c,l,v,r) =>
                        (case compare(x, v)
                            of LESS => (lp l; balance t)
                             | GREATER => (lp r; balance t)
                             | EQUAL => (lp r; balance t))
                     | DBL => raise Fail "found double black leaf in insert\n"
        in atomic(fn () => (lp t; makeBlack t)) end

    fun isBlack t = 
        case get t
            of T(Black,_,_,_) => true
             | L => true
             | _ => false
        
    fun bubble t = 
        case get t
            of T(c,l,x,r) =>
                if isBB l orelse isBB r
                then (put(t, T(blacker c, l, x, r)); redder' l; redder' r; balance t)
                else balance t
             | _ => ()

    (*Precondition: t has only one child. *)
    fun remove' t = 
        case get t
            of T(Red,l,v,r) => put(t, L)    (*l anv v are necessarily L*)
             | T(Black,l,v,r) =>    
                (case get l
                    of T(Red,a,x,b) => put(t, T(Black,a,x,b))
                     | _ => case get r
                                of T(Red, a, x, b) => put(t, T(Black, a, x, b))
                                 | _ => put(t, DBL))
             | _ => raise Fail "Impossible: removePrime"

    exception NotFound of int
    fun remove compare x t = 
        let fun removeMax t = 
                case get t
                    of T(c,l,v,r) => 
                        (case get r
                            of L => (remove' t; v)
                             | _ => let val v = removeMax r
                                        val _ = bubble t
                                    in v end)
                     | _ => raise Fail "Impossible: remove"
            fun lp t = 
                case get t
                    of L => ()
                     | T(c,l,v,r) => 
                        (case compare(x, v)
                            of GREATER => (lp r; bubble t)
                             | LESS => (lp l; bubble t)
                             | EQUAL => 
                                (case get l
                                    of L => remove' t
                                     | _ => (case get r
                                                of L => remove' t
                                                 | _ => put(t, T(c,l,removeMax l,r)); bubble t)))
                     | DBL => raise Fail "found double black leaf in remove:lp\n"                     
        in atomic(fn _ => (lp t ; makeBlack t; true) handle NotFound x => false)
        end

    (*Verify red-black tree properties*)
    fun chkOrder comp t = 
        let fun lp(t, lower, upper) = 
                case unsafeGet t
                    of L => true
                     | T(c,l,v,r) =>   
                        let val b1 = lp(l, lower, SOME v)
                            val b2 = lp(r, SOME v, upper)
                        in case (lower, upper)
                            of (NONE, NONE) => true
                             | (NONE, SOME u) => (case comp(v, u) of LESS => true | _ => false)
                             | (SOME l, NONE) => (case comp(v, l) of LESS => false | _ => true)
                             | (SOME l, SOME u) => (case (comp(v,l), comp(v,u)) 
                                                        of (EQUAL, LESS) => true
                                                         | (GREATER, LESS) => true
                                                         | _ => false)
                        end
                     | DBL => raise Fail "found double black leaf in chkOrder\n"
        in if lp(t, NONE, NONE) 
           then print "Red black tree order is correct\n" 
           else print "Red black tree order is incorrect\n"
        end

    datatype expected = MustBeBlack | Any

    fun chkBlackPaths t = 
        let fun lp(t, exp, d) =
                case (exp, unsafeGet t)
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

    fun depth t = 
        let fun lp t =  
            case get t
                of T(c,l,v,r) => 1 + Int.max(lp l, lp r)
                 | _ => 0
        in atomic(fn () => lp t) end

    fun unsafeDepth t = 
	case unsafeGet t
	 of T(c,l,v,r) => 1 + Int.max(unsafeDepth l, unsafeDepth r)
	  | _ => 0

    fun numNodes t = 
	case unsafeGet t
	 of T(c,l,v,r) => 1 + numNodes l + numNodes r
	  | DBL => (print "Found double black leaf in numNodes!\n"; 0)
	  | _ => 0
	

end

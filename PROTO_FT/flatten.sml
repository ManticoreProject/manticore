structure Flatten : sig

  type FArray
  type NFArray

  val mkFlatExp : Expression.exp -> Expression.exp

  end = struct

    type var = Var.var
    type ty = Var.ty

    (* Shape tree. We can rethink this later. *)
    datatype Shape = Node of Shape list
                   | Lf of int * int

    (*We need ty in case the array is empty.*)
    datatype FArray = FArray of var array * ty * Shape

    datatype NFArray = NFA_Tup of NFArray * NFArray
                     | NFA_Arr of NFArray array
                     | NFA_Lf of FArray

    type exp = Expression.exp

    fun mkFlatExp e =
      case e
        of Let(v,e) => raise Fail "todo"
         | Case(v,ees) => raise Fail "todo"
         | Apply(v,e) => raise Fail "todo"
         | Statement(v) => raise Fail "todo"
         | _ => raise Fail "Invalid expression in Flatten.mkFlatExp"

  end

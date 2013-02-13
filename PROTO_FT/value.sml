structure Var :> sig

    type ty
    type term
    type var
    type exp

    structure IDTbl

    val newVar : term * string -> var

    end = struct

        datatype ty = ARR_TY of ty
                    | FUN_TY of ty * ty
                    | TUP_TY of ty * ty
                    | INT_TY
                    | UNIT_TY

        and term = ARR of term array
                 | FUN of term -> term
                 | TUP of term * term
                 | INT of int
                 | UNIT

        and var = V of {
            id : word,
            value : term,
            ty : ty,
            name : Atom
            }

        datatype exp = Let of (var * exp)
                     | Case of (var * (exp * exp) list)
                     | Apply of (var * exp)
                     | Statement of var

        structure Key =
        structure IDTbl = HashTableFn(
            struct
                type hash_key = var
                val hashVal = fn (V{id, ...}) => id
                val sameKey (V{id,...},V{id=id',...}) = (id = id')
            end
        )

        val cnt = ref 0w0
        fun nextID () = (cnt:=!cnt+0w1; !cnt)

        fun newVar (t,name) = V{
            id = nextID(),
            value = t,
            ty = tyOf t,
            name = Atom.atom name
            }

    end

structure Var :> sig

    type ty
    type term
    type var

    structure IDTbl : MONO_HASH_TABLE where type Key.hash_key = var

    val newVar : term * ty * string -> var

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
        name : Atom.atom
        }

      structure IDTbl = HashTableFn(
        struct
            type hash_key = var
            val hashVal = fn (V{id, ...}) => id
            fun sameKey (V{id,...},V{id=id',...}) = (id = id')
        end
      )

      val cnt = ref 0w0
      fun nextID () = (cnt:=(!cnt)+0w1; !cnt)

      fun newVar (t,ty,name) = V{
        id = nextID(),
        value = t,
        ty = ty,
        name = Atom.atom name
        }

    end

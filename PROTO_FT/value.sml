structure PROTOVal =
  struct

    type id = word

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
        id : id,
        value : term,
        ty : ty,
        }

    datatype exp = Let of (value * exp)

  end

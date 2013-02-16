structure Expression : sig
    type exp

    end = struct

      type var = Var.var

      datatype exp = Let of (var * exp)
                   | Case of (var * (exp * exp) list)
                   | Apply of (var * exp)
                   | Statement of var

    end

(* Temporary basis that will compile and pass tests immediately. *)
structure Word64 =
   struct
      _type t = _prim (int32)
      type word = t
   end

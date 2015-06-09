(* Temporary basis that will compile and pass tests immediately. *)

_module W64 _prim(
  fun add64 (x : int64, y : int64 / exh : cont<exn>) -> int64 =
        let z = I64Add(x, y)
	return (z);
  fun sub64 (x : int64, y : int64 / exh : cont<exn>) -> int64 =
        let z = I64Sub(x, y)
	return (z);
)

structure Word64 =
   struct
      _type t = _prim (int64)
      type word = t
      _val add : word * word -> word = _prim(W64.add64)
      _val subtract : word * word -> word = _prim(W64.sub64)
   end

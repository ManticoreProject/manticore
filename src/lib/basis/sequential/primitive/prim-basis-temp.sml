(* Temporary basis that will compile and pass tests immediately. *)


_module W64
  _import datatype bool with
    _con true
    _con false
  end
  _prim(
    fun add64 (x : int64, y : int64 / exh : cont<exn>) -> int64 =
        let z = I64Add(x, y)
        return (z);
    fun sub64 (x : int64, y : int64 / exh : cont<exn>) -> int64 =
        let z = I64Sub(x, y)
        return (z);
    fun mul64 (x : int64, y : int64 / exh : cont<exn>) -> int64 =
        let z = I64Mul(x, y)
        return (z);
    fun eq64 (x : int64, y : int64 / exh : cont<exn>) -> bool =
        if I64Eq(x, y) then return (true) else return (false);
)

structure Word64 =
   struct
      _type t = _prim (int64)
      type word = t
      _val + : word * word -> word = _prim(W64.add64)
      _val - : word * word -> word = _prim(W64.sub64)
      _val * : word * word -> word = _prim(W64.mul64)
      _val = : word * word -> bool = _prim(W64.eq64)
   end

val op+ = Word64.+
val op- = Word64.-
val op= = Word64.=

_module W32
  _import datatype bool with
    _con true
    _con false
  end
  _prim(
  fun add32 (x : int32, y : int32 / exh : cont<exn>) -> int32 =
        let z = I32Add(x, y)
	return (z);
  fun sub32 (x : int32, y : int32 / exh : cont<exn>) -> int32 =
        let z = I32Sub(x, y)
	return (z);
  fun mul32 (x : int32, y : int32 / exh : cont<exn>) -> int32 =
        let z = I32Mul(x, y)
        return (z);
  fun eq32 (x : int32, y : int32 / exh : cont<exn>) -> bool =
        if I32Eq(x, y) then return (true) else return (false);
)

structure Word32 =
   struct
      _type t = _prim (int32)
      type word = t
      _val + : word * word -> word = _prim(W32.add32)
      _val - : word * word -> word = _prim(W32.sub32)
      _val * : word * word -> word = _prim(W32.mul32)
      _val = : word * word -> word = _prim(W32.eq32)
   end


_overload 2 * : 'a * 'a -> 'a
as (* Int.*
and IntInf.*
and LargeInt.*
and FixedInt.*
and Position.*
and Int32.*
and Int64.*
and Word.*
and LargeWord.*
and SysWord.*
and *) Word32.*
and Word64.*
(*and Real.*
and Real32.*
and Real64.*
and LargeReal.**)



structure Primitive =
struct

(* chars! *)
structure Char8 =
   struct
      type t = char8
      type char = t
   end
structure Char16 =
   struct
      type t = char16
      type char = t
   end
structure Char32 =
   struct
      type t = char32
      type char = t
   end

structure Bool =
   struct
      datatype t = datatype bool
      datatype bool = datatype t
   end
structure Exn =
   struct
      type t = exn
      type exn = t
      exception Bind = Bind
      exception Match = Match
      exception PrimOverflow = Overflow
   end
structure List =
   struct
      datatype t = datatype list
      datatype list = datatype t
   end

structure Unit =
   struct
      type t = unit
      type unit = t
   end

structure Array =
   struct
      type 'a t = 'a array
      type 'a array = 'a t
   end
structure Vector =
   struct
      type 'a t = 'a vector
      type 'a vector = 'a t
   end


(*structure String =
   struct
      open CharVector
      type string = vector

      val size = length
      val op ^ = append
      val implode = fromList
      val new = vector
   end*)

structure String8 =
   struct
      type t = Char8.t vector
      type string = t
   end


end (*structure Primitive*)


(*infixes*)
infix  7 * / mod div
infix  6 + - ^
infixr 5 :: @
infix  4 = <> > >= < <=
infix  3 := o
infix  0 before

(* Top-level bindings *)
type char = Primitive.Char8.t

datatype bool = datatype Primitive.Bool.bool
type exn = Primitive.Exn.exn
datatype list = datatype Primitive.List.list
(*datatype ref = datatype Primitive.Ref.ref*)
type unit = Primitive.Unit.unit
type 'a array = 'a Primitive.Array.array
type 'a vector = 'a Primitive.Vector.vector

type string = Primitive.String8.string

exception Fail of string

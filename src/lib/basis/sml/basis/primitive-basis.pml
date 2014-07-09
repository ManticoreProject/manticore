(* primitive-basis.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Bind ML names to the primitive types that are predefined in BOM.
 *)

structure Primitive =
  struct

    structure Bool =
      struct
	_datatype bool = _prim(bool)
      end

    structure Exn =
      struct
	_type exp = _prim(exn)
	exception Bind = Bind
	exception Match = Match
      end

    structure List =
      struct
	_datatype 'a list = _prim(list<'a>)
      end

    structure Unit =
      struct
	_type unit = _prim(unit)
      end

  end

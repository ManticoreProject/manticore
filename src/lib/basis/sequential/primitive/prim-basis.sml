(* prim-basis.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This code is adapted from the MLton sources.
 *)

(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Primitive = struct

(* Primitive Basis (Definition) *)
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
structure Ref =
   struct
      datatype t = datatype ref
      datatype ref = datatype t
   end
structure Unit =
   struct
      type t = unit
      type unit = t
   end

(* Primitive Basis (Basis Library) *)
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

(* Primitive Basis (Primitive Types) *)
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

structure Int8 =
   struct
      type t = int8
      type int = t
   end
structure Int16 =
   struct
      type t = int16
      type int = t
   end
structure Int32 =
   struct
      type t = int32
      type int = t
   end
structure Int64 =
   struct
      type t = int64
      type int = t
   end
structure IntInf =
   struct
      type t = intInf
      type int = t
   end

structure Real32 =
   struct
      type t = real32
      type real = t
   end
structure Real64 =
   struct
      type t = real64
      type real = t
   end

structure String8 =
   struct
      type t = Char8.t vector
      type string = t
   end
structure String16 =
   struct
      type t = Char16.t vector
      type string = t
   end
structure String32 =
   struct
      type t = Char32.t vector
      type string = t
   end

structure Word1 =
   struct
      type t = word1
      type word = t
   end
structure Word8 =
   struct
      type t = word8
      type word = t
   end
structure Word16 =
   struct
      type t = word16
      type word = t
   end
structure Word32 =
   struct
      type t = word32
      type word = t
   end
structure Word64 =
   struct
      type t = word64
      type word = t
   end

(* Primitive Basis (MLton Extensions) *)
structure Pointer =
   struct
      type t = cpointer
   end
structure Thread =
   struct
      type t = thread
   end
structure Weak =
   struct
      type 'a t = 'a weak
   end

end

(* Top-level bindings *)
datatype bool = datatype Primitive.Bool.bool
type exn = Primitive.Exn.exn
datatype list = datatype Primitive.List.list
datatype ref = datatype Primitive.Ref.ref
type unit = Primitive.Unit.unit
type 'a array = 'a Primitive.Array.array
type 'a vector = 'a Primitive.Vector.vector

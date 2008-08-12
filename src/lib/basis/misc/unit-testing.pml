(* unit-testing.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure UnitTesting =
  struct

    structure PT = PrimTypes

    fun fib n = if n < 2 then n else fib(n-1) + fib(n-2)

    fun validate s f = if f() then () else fail s

  end

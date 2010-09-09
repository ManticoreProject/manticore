(* unit-testing.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure UnitTesting =
  struct

    structure PT = PrimTypes

    fun validate s f = 
	  if f() 
	     then Print.printLn "success" 
	  else Print.printLn("[ERROR] unit test failed: "^s)

  end

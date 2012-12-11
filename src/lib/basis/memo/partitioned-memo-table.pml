(*
 * The partitioned version of the memo table assigns each entry to
 * a particular processor based on its key modulo the leaf size.
 *)
structure PartitionedMemoTable =
  struct

  type 'a table = int * int * 'a option Array.array option Array.array

  fun mkTable max = 
      (max, (max div VProcUtils.numNodes()) + 1, 
       Array.array (VProcUtils.numNodes (), NONE))


  fun insert ((max, leafSize, arr), key, item) = (
      if (key >= max)
      then raise Fail "Index out of range"
      else ();
      case Array.sub (arr, key div leafSize)
       of NONE => (let
                      val newarr = Array.array (leafSize, NONE)
                      val _ = Array.update (newarr, key mod leafSize, SOME item)
                  in
                      Array.update (arr, key div leafSize, SOME newarr)
                  end)
        | SOME internal => (
            Array.update (internal, key mod leafSize, SOME item)))

  fun find ((max, leafSize, arr), key) = (
      if (key >= max)
      then raise Fail "Index out of range"
      else ();
      case Array.sub (arr, key div leafSize)
        of NONE => NONE
         | SOME internal => (Array.sub (internal, key mod leafSize)))

  end

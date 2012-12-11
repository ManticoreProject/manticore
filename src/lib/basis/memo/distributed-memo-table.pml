(*
 * The distributed version of the memo table does not duplicate entries
 * between nodes, but instead caches only locally-produced values.
 *)

structure DistributedMemoTable =
  struct

  type 'a table = int * 'a option Array.array option Array.array

  fun mkTable max =
      (max, Array.array (VProcUtils.numNodes (), NONE))


  fun insert ((max, arr), key, item) = (
      if (key >= max)
      then raise Fail "Index out of range"
      else ();
      case Array.sub (arr, VProcUtils.node())
       of NONE => (let
                      val newarr = Array.array (max, NONE)
                      val _ = Array.update (newarr, key, SOME item)
                  in
                      Array.update (arr, VProcUtils.node (), SOME newarr)
                  end)
        | SOME internal => (
            Array.update (internal, key, SOME item)))

  fun find ((max, arr), key) = (
      if (key >= max)
      then raise Fail "Index out of range"
      else ();
      (case Array.sub (arr, VProcUtils.node())
        of NONE => NONE
         | SOME internal => Array.sub (internal, key)))

  end

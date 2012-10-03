structure MemoTable =
  struct

  type 'a table = int * 'a option Array.array

  fun mkTable max =
      (max, Array.array (max, NONE))


  fun insert ((max, arr), key, item) = (
      if (key >= max)
      then raise Fail "Index out of range"
      else ();
      Array.update (arr, key, SOME item))

  fun find ((max, arr), key) = (
      if (key >= max)
      then raise Fail "Index out of range"
      else ();
      Array.sub (arr, key))

  end

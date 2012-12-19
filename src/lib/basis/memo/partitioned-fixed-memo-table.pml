(*
 * The partitioned version of the memo table assigns each entry to
 * a particular processor based on its key modulo the leaf size.
 * Further, it uses a fixed sized designed to fit easily within a
 * global heap segment.
 *)
structure PartitionedFixedMemoTable =
  struct

  type 'a table = int * 'a option Array.array option Array.array

  datatype 'a entry = ENTRY of long * int * 'a

  val max = 256*1024
  val nEntries = 5

  fun mkTable () = 
      ((max div VProcUtils.numNodes()) + 1, 
       Array.array (VProcUtils.numNodes (), NONE))

  (* TODO: will probably need to improve Time.now to not make a C call... *)
  fun insert ((leafSize, arr), key, item) = (
      if (key >= max)
      then raise Fail "Index out of range"
      else ();
      let
          val age = Time.now()
          val new = ENTRY (age, key, item)
          val subarray = (case Array.sub (arr, key div leafSize)
                           of NONE => (let
                                          val newarr = Array.array (leafSize, NONE)
                                          val _ = Array.update (arr, key div leafSize, SOME newarr)
                                      in
                                          newarr
                                      end)
                            | SOME arr => arr)
          val startIndex = (key mod leafSize) * nEntries
          fun insertEntry (i, oldestTime, oldestIndex) = (
              if i = nEntries
              then Array.update (subarray, oldestIndex, SOME new)
              else (case Array.sub (subarray, startIndex + i)
                     of NONE => Array.update (subarray, startIndex + i, SOME new)
                      | SOME (ENTRY (t, _, _)) =>
                        if t < oldestTime
                        then insertEntry (i+1, t, i)
                        else insertEntry (i+1, oldestTime, oldestIndex)))
      in
          insertEntry (0, Int.toLong (Option.valOf Int.maxInt), 0)
      end)

  fun find ((leafSize, arr), key) = (
      if (key >= max)
      then raise Fail "Index out of range"
      else ();
      case Array.sub (arr, key div leafSize)
        of NONE => NONE
         | SOME internal => (
             let
                 val startIndex = (key mod leafSize) * nEntries
                 fun findEntry (i) = (
                     if (i = nEntries)
                     then NONE 
                     else (let
                              val e = Array.sub (internal, startIndex + i)
                          in
                              case e
                               of NONE => findEntry (i+1)
                                | SOME (ENTRY(_, key', value)) =>
                                  if key' = key
                                  then (Array.update (internal, startIndex+i, SOME (ENTRY(Time.now(), key', value)));
                                        SOME value)
                                  else (findEntry (i+1))
                          end))
             in
                 findEntry 0
             end))
  end

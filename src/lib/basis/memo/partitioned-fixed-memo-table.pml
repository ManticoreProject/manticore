(*
 * The partitioned version of the memo table assigns each entry to
 * a particular processor based on its key modulo the leaf size.
 * Further, it uses a fixed sized designed to fit easily within a
 * global heap segment.
 *)
structure PartitionedFixedMemoTable =
  struct

  (* (Number of nodes, elements per node, buckets per element, array of arrays) *)
  type 'a table = int * int * int * 'a option Array.array option Array.array

  datatype 'a entry = ENTRY of long * int * 'a

(*  val max = 1024*32
  val nEntries = 10 *)

  fun mkTable (max, nEntries) =
      (VProcUtils.numNodes (), max, nEntries,
       (Array.array (VProcUtils.numNodes (), NONE)))

  (* TODO: will probably need to improve Time.now to not make a C call... *)
  fun insert ((buckets, max, nEntries, arr), key, item) = (
      let
          val age = Time.now()
          val new = ENTRY (age, key, item)
          val subarray = (case Array.sub (arr, key mod buckets)
                           of NONE => (let
                                          val newarr = Array.array (max * nEntries, NONE)
(*                                          val _ = print (String.concat["Array size: ", Int.toString (max *nEntries), ", for node:", Int.toString (key mod buckets),
								     " with bucket-count: ", Int.toString buckets, "\n"]) *)
                                          val _ = Array.update (arr, key mod buckets, SOME newarr)
                                      in
                                          newarr
                                      end)
                            | SOME arr => arr)
          val startIndex = ((key div buckets) mod max) * nEntries
          fun insertEntry (i, oldestTime, oldestOffset) = (
              if i = nEntries
              then (Array.update (subarray, startIndex + oldestOffset, SOME new))
              else (case Array.sub (subarray, startIndex + i)
                     of NONE => (Array.update (subarray, startIndex + i, SOME new))
                      | SOME (ENTRY (t, _, _)) =>
                        if t < oldestTime
                        then insertEntry (i+1, t, i)
                        else insertEntry (i+1, oldestTime, oldestOffset)))
      in
          insertEntry (0, Int.toLong (Option.valOf Int.maxInt), 0)
      end)

  fun find ((buckets, max, nEntries, arr), key) = (
      case Array.sub (arr, key mod buckets)
        of NONE => NONE
         | SOME internal => (
             let
                 val startIndex = ((key div buckets) mod max) * nEntries
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

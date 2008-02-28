  fun parString a =
    let val len = plen a
        fun build (curr, acc) =
          if curr=len
          then rev acc
          else build (curr+1, (itos (a!curr)) :: acc)
    in
        "[|" ^ (concatWith (",", build (0, nil))) ^ "|]"
    end;

  val nums = [| 1 to 1000 |];

  val numsPart = psubseq (nums, 1, 2);

  (print ("numsPart: ");
   print (parString numsPart);
   print ("\n"))


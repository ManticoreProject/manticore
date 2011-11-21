functor MapSum (
  structure R : ROPE
) = struct

  fun add (x, y) = x + y

  fun mapSum rp = R.map (fn rp' => R.reduce add rp') rp

  fun gen n = R.tabulate (n, fn i => R.tabulate (i, fn j => j))

end

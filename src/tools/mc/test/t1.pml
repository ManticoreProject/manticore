signature S =
  sig
    val x : int
  end

structure S : S =
  struct 
    val x = 34
  end

val x = 34
val z = S.x

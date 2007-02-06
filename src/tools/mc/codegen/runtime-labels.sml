structure RuntimeLabels = struct

  local val global = Label.global
  in
    val initGC = global "asm_init_gc"
  end (* local *)

end (* RuntimeLabels *)

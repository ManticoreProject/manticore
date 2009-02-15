signature LAZY_FUTURE =
  sig

    type 'a future

  (* create a future *)
    val delay : (unit -> 'a) -> 'a future
  (* place the future on the current work group ready queue *)
    val run : 'a future -> unit
  (* synchronize on completion of the future *)
    val force : 'a future -> 'a
  (* make the future cancelable *)
    val cancelable : 'a future -> 'a future
  (* cancel the future *)
    val cancel : 'a future -> unit

  end

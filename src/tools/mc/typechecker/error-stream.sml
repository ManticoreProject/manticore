structure ErrorStream =
  struct

  (* FIXME: the following is a hack to avoid threading the error stream through
   * all of the typechecking code.  Eventually, we should fix this, since otherwise
   * it is a space leak.
   *)
    val errStrm = ref(Error.mkErrStream "<bogus>")

    fun setErrStrm es = errStrm := es

    fun error (span, msg) = Error.errorAt (!errStrm, span, msg)

  end

(* time.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure Time =
  struct

    structure PT = PrimTypes

  (* time is in microseconds *)
    type time = long

    _primcode(
      typedef time = long;

      extern void* M_GetTime();

    (* get the current time *)
      define inline @now( / exh : PT.exh) : time =
        let t : long = ccall M_GetTime()
        return(t)
      ;

      define inline @now-wrapper(x : PT.unit / exh : PT.exh) : [time] =
        let t : time = @now(/exh)      
        return(alloc(t))
      ;

      define inline @lte (x : time, y : time / exh : PT.exh) : PT.bool =
        return(I64Lte(x, y))
      ;

      define inline @gt (x : time, y : time / exh : PT.exh) : PT.bool =
        return(I64Gt(x, y))
      ;

      define inline @add (x : time, y : time / exh : PT.exh) : time =
        return(I64Add(x, y))
      ;

    )

    val now : unit -> time = _prim(@now-wrapper)

    fun fromSecs t = t * 1000000
    fun toSecs t = t div 1000000

  end

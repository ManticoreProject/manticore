(* time.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure Time =
  struct

  (* time is in microseconds *)
    type time = long

    _primcode(

      typedef time = long;
      typedef ml_time = [time];

      extern long M_GetTime() __attribute__((pure));
      extern double M_GetCPUTime() __attribute__((pure));

    (* get the current time *)
      define inline @now () : time =
	 let t : long = ccall M_GetTime()
	 return (t)
       ;

      define inline @now-w (_ : unit / exh : exh) : ml_time =
	  let t : time = @now()
	  return (alloc(t))
	;

      define inline @cpu-time (_ : unit / exh : exh) : ml_double =
	  let t : double = ccall M_GetCPUTime()
	  return (alloc(t))
	;

    )

    val now : unit -> time = _prim(@now-w)
    val cpuTime : unit -> double = _prim(@cpu-time)

    fun fromSecs t = t * 1000000
    fun toSecs t = t div 1000000

    fun toString (t : time) = let
	   val (sign, t) = if (t < 0) then ("-", ~t) else ("", t)
	   val ms = (t div 1000) mod 1000	(* milliseconds *)
	   val frac = if (ms < 10) then ".00" ^ Long.toString ms
		 else if (ms < 100) then ".0" ^ Long.toString ms
		 else "." ^ Long.toString ms
	   in
	     sign ^ Long.toString(toSecs t) ^ frac
	   end

(* FIXME: this function does not belong here!!! *)
  (* timeToEval : (unit -> 'a) -> 'a * time *)
  (* Pass in a suspended computation; get back the result and the time it took. *)
    fun timeToEval f = let
	   val b = now()
	   val x = f()
	   val e = now()
	   in
	      (x, e-b)
	   end

  end

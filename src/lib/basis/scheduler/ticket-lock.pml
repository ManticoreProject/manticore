structure TicketLock = struct
  _primcode (

  typedef ticket_lock = 
                ![
	           long,      (* current value *)
	           long       (* ticket counter *)
	         ];

  define @create (_ : unit / exh : exh) : ticket_lock =
      let l : ticket_lock = alloc (0:long, 0:long)
      let l : ticket_lock = promote(l)
      return(l)
    ;

  define @lock (l : ticket_lock / exh : exh) : ml_long =
      let ticket : long = I64FetchAndAdd (&1(l), 1:long)
      let p : ml_long = alloc(ticket)
      fun spinLp () : ml_long =
          let difference : long = I64Sub(ticket,#0(l))
          if I64Eq(difference, 0:long)
	  then
              return (p)
          else
              let t : long = I64Mul(difference, 100000:long)
              do SchedulerAction.@sleep (t)
	      apply spinLp ()
      apply spinLp()
    ;
    
  define @get_ticket (l : ticket_lock / exh : exh) : ml_long =
      let ticket : long = I64FetchAndAdd (&1(l), 1:long)
      let p : ml_long = alloc(ticket)
      return (p)
    ;

  define @current_ticket (l : ticket_lock / exh : exh) : ml_long =
      let ticket : long = #0(l)
      let p : ml_long = alloc(ticket)
      return (p)
    ;

  define @lock_with_ticket (l : ticket_lock, ticket : ml_long  / exh : exh) : unit =
      let ticket : long = #0(ticket)
      fun spinLp () : unit =
          let difference : long = I64Sub(ticket,#0(l))
          if I64Eq(difference, 0:long)
	  then
              return (enum(0):PrimTypes.unit)
          else
              let t : long = I64Mul(difference, 100000:long)
              do SchedulerAction.@sleep (t)
	      apply spinLp ()
      apply spinLp()
    ;
  define @lock_with_ticket-w (arg : [ticket_lock, ml_long] / exh : exh) : unit =
    @lock_with_ticket (#0(arg), #1(arg) / exh)
  ;

  define @unlock (l : ticket_lock, t : ml_long / exh : exh) : unit =
       let t : long = #0(t)
       let t : long = I64Add(t,1:long)
       do #0(l) := t
       return (enum(0):PrimTypes.unit)
    ;
  define @unlock-w (arg : [ticket_lock, ml_long] / exh : exh) : unit =
    @unlock (#0(arg), #1(arg) / exh)
  ;
  )                                                        

  type ticket_lock = _prim(ticket_lock)
  val create : unit -> ticket_lock = _prim(@create)
  val lock : ticket_lock -> long = _prim(@lock)
  val getTicket : ticket_lock -> long = _prim(@get_ticket)
  val currentTicket : ticket_lock -> long = _prim(@current_ticket)
  val lockWithTicket : ticket_lock * long -> unit = _prim(@lock_with_ticket-w)
  val unlock : ticket_lock * long -> unit = _prim(@unlock-w)
end


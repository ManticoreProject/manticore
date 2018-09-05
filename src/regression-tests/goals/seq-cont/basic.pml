

fun retEarly k = (
  print "about to throw\n" ;
  Cont.throw (k, ()) ;
  print "ERROR!\n"
  )

fun retNormal k = print "returning\n"

fun skipOver k1 = let
  fun inner k2 = (
    print "at inner's throw\n" ;
    Cont.throw (k1, ()) ;
    print "ERROR!\n"
    )
  in (
    print "at callec of inner\n" ;
    Cont.callec inner ;
    print "ERROR!\n"
    )
  end




val _ = (print "before retEarly\n" ; Cont.callec retEarly ; print "after retEarly\n")
val _ = print "\n==\n"

val _ = (print "before retNormal\n" ; Cont.callec retNormal ; print "after retNormal\n")
val _ = print "\n==\n"

val _ = (print "before skipOver\n" ; Cont.callec skipOver ; print "after skipOver\n")

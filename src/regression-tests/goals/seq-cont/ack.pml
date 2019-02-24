fun ack(m,n) k = if m=0 then
                   Cont.throw(k, n+1)
               else if m>0 andalso n=0 then
                   ack(m-1,1) k
               else if m>0 andalso n>0 then
                   ack(m-1,Cont.callec( ack(m,n-1) )) k
               else
                   raise Fail "undefined"

val _ = Print.printLn (Int.toString (Cont.callec(ack (3, 1)))) (* should be 13 *)

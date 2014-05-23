

exception Test of string

val _ = (raise Test "test exception being raised\n") handle Test s => print s






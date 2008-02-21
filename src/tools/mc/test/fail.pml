fun hd l = (case l
       of nil => fail "error"
	| x::xs => x);

hd(nil)

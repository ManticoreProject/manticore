(* FAIL [varpat missing type annotation] *)
_module myId (
 fun myFun<'a> (x) -> 'a = return(x);
)

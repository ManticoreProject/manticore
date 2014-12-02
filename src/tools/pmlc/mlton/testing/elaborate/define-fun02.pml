(* FAIL [varpat missing type annotation] *)
_module myId _prim (
 fun myFun<'a> (x) -> 'a = return(x);
)

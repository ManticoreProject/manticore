(* FAIL [undefined type] *)
_module myId (
  type myType <'a, 'b> = ['a, 'b];
  type myType' = anUndefinedType<int32, int32>;
)

(* FAIL [labels must be strictly increasing] *)
_module myId _prim (
  type myType<'a> = ['a, 'a];
  type myFieldTy = {
    2 ! myType<int32>,
    1 ! myType<int64>
  };
)

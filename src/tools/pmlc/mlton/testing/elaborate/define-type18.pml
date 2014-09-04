(* FAIL [labels must be strictly increasing] *)
_module myId (
  type myType<'a> = ['a, 'a];
  type myFieldTy = {
    1 ! myType<int32>,
    1 ! myType<int64>
  };
)

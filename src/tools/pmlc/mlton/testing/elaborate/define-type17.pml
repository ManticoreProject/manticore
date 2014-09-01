_module myId (
  type myType<'a> = ['a, 'a];
  type myFieldTy = {
    1 ! myType<int32>,
    2 ! myType<int64>
  };
)

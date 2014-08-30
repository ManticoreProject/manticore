_module myModule (
  type myType <'a> = ['a, 'a];
)

_module myOtherModule (
  type myType <'a, 'b> = ['a, 'b];
  type myOtherType = myModule.myType<int32>;
)

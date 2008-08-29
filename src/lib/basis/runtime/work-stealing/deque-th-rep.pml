structure DequeTHRep =
  struct

    structure PT = PrimTypes
    structure Arr = Array64

    _primcode (

      typedef deque = ![
		  int,                   (* T *)
		  int,                   (* H *)
		  Arr.array,             (* deque memory *)
		  PT.bool                (* lock *)
	      ];
    )

  end

structure Progress = struct
datatype ('a, 'b) progress
  = More of 'a
  | Done of 'b
end

signature RUNTIME = sig
val hungryProcs : unit -> bool
val numProcs : unit -> int
val par2 : (unit -> 'a) * (unit -> 'b) -> 'a * 'b
val parN : (unit -> 'a) list -> 'a list
end

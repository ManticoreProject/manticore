signature SOURCE_MAP = sig
    val getMap : unit -> AntlrStreamPos.sourcemap
    val setMap : AntlrStreamPos.sourcemap -> unit
end

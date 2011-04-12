(* basis-items.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Items from the basis needed for the flattening transformation's implementation.
 *
 *)

structure BasisItems : sig

  val farrayTyc      : unit -> Types.tycon
  val nestingTreeTyc : unit -> Types.tycon
  val farrayCon      : unit -> Types.dcon
  val lfCon          : unit -> Types.dcon
  val ndCon          : unit -> Types.dcon

  val basisItems : unit -> {farrayTyc      : Types.tycon,
			    nestingTreeTyc : Types.tycon,
			    farrayCon      : Types.dcon,
			    lfCon          : Types.dcon,
			    ndCon          : Types.dcon}
  
end = struct

  fun memo get path = let
    val cell = ref NONE
    fun record x = (cell := SOME x; x)
    fun read () = (case !cell
      of SOME x => x
       | NONE => record (get path)
      (* end case *))
    in
      read 
    end

  val tyc = memo BasisEnv.getTyConFromBasis
  val dcon = memo BasisEnv.getDConFromBasis

  val farrayTyc      = tyc ["FArray", "f_array"]
  val nestingTreeTyc = tyc ["FArray", "nesting_tree"]

  val farrayCon      = dcon ["FArray", "FArray"]
  val lfCon          = dcon ["FArray", "Lf"]
  val ndCon          = dcon ["FArray", "Nd"]

  fun basisItems _ = {farrayTyc = farrayTyc (),
		      nestingTreeTyc = nestingTreeTyc (),
		      farrayCon = farrayCon (),
		      lfCon = lfCon (),
		      ndCon = ndCon ()}

end

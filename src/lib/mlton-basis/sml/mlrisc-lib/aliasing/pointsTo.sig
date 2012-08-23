(*
 *  This module can be used to perform points-to analysis for typed languages
 *
 * -- Allen
 *)
signature POINTS_TO =
sig

   eqtype edgekind 
   structure C : CELLS_BASIS (* = CellsBasis *)
                 where type CellSet.cellset = CellsBasis.CellSet.cellset
                   and type 'a ColorTable.hash_table = 'a CellsBasis.ColorTable.hash_table
                   and type 'a HashTable.hash_table = 'a CellsBasis.HashTable.hash_table
                   and type SortedCells.sorted_cells = CellsBasis.SortedCells.sorted_cells
                   and type cell = CellsBasis.cell
                   and type cellColor = CellsBasis.cellColor
                   and type cellkind = CellsBasis.cellkind
                   and type cellkindDesc = CellsBasis.cellkindDesc
                   and type cellkindInfo = CellsBasis.cellkindInfo

   datatype cell = 
     LINK  of cell ref
   | SREF  of C.cell * (edgekind * int * cell ref) list ref
   | WREF  of C.cell * (edgekind * int * cell ref) list ref
   | SCELL of C.cell * (edgekind * int * cell ref) list ref
   | WCELL of C.cell * (edgekind * int * cell ref) list ref
   | TOP   of {mutable:bool, id:C.cell, name:string}
      (* a collapsed node *)
   type region = cell ref
   type edges  = (edgekind * int * region) list

   val reset    : (unit -> C.cell) -> unit

   (* generate a new reference/immutable cell *)
   val newSRef  : unit -> region  
   val newWRef  : unit -> region  
   val newSCell : unit -> region  
   val newWCell : unit -> region  

   (* generate a new collapsed node *)
   val newTop   : {mutable:bool,name:string} -> region  

   (*  
    * The following are methods for constructing the storage shape graph.
    *)
   val pi     : region * int -> region (* the ith projection *)
   val sub    : region * int -> region (* the ith subscript *)
   val dom    : region * int -> region (* the ith domain *)
   val ran    : region * int -> region (* the ith range *)
   val offset : region * int -> region (* the ith offset *)

   val unify     : region * region -> unit 
   val interfere : region * region -> bool (* do they interfere? *) 

   (*   
    * More complex methods
    *)
   val mkRecord : region option * region list -> region    
   val mkRef    : region option * region -> region        
   val mkArray  : region option * region list -> region
   val mkVector : region option * region list -> region
   val mkLambda : region list -> region (* define a function *)

   val app      : region * region list -> unit (* apply a function *)
   val ret      : region * region list -> unit (* binds the return values *)

   val strongUpdate    : region * int * region -> unit
   val strongSubscript : region * int -> region 
   val weakUpdate      : region * region -> unit
   val weakSubscript   : region -> region

   val toString  : region -> string

end

(* basic-control.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Basic controls for the mc compiler.
 *)

structure BasicControl : 
sig
   (* the top-level registery of the compiler *)
   val topregistry : ControlRegistry.registry
   (* nest a tier-2 registery within the top-level registery *)
   val nest : string * ControlRegistry.registry * Controls.priority -> unit

   val debugPriority : int
   (* *)
   val show_all : (string -> unit) -> 
                  (({ctl: string Controls.control,
                     info: ControlRegistry.control_info} -> string) *
                   ({ctl: string Controls.control,
                     info: ControlRegistry.control_info} -> string)) ->
                  int option ->
                  unit
end =
struct
   val topregistry = ControlRegistry.new {help = "mc controls"}

   fun nest (prefix, reg, pri) =
      ControlRegistry.nest topregistry {prefix = SOME prefix,
                                        pri = pri,
                                        obscurity = 0,
                                        reg = reg}

   val debugPriority = 2

   fun show_all output (getarg, getvalue) level =
      let
         fun walk indent (ControlRegistry.RTree rt) =
            let
               val sp = CharVector.tabulate (indent, fn _ => #" ")
               val sp' = CharVector.tabulate (indent + 1, fn _ => #" ")
               val {help, ctls, subregs, path} = rt
               fun one ci =
                  let
                     val arg = concat (foldr (fn (s, r) => s :: "." :: r)
                                             [getarg ci]
                                             path)
                     val value = getvalue ci
                  in
                     output (concat [sp',arg," : ",value,"\n"])
                  end
            in
               (output (concat [sp, help, ":\n"])
                ; app one ctls
                ; app (walk (indent + 2)) subregs)
            end
      in
         walk 2 (ControlRegistry.controls (topregistry, level))
      end
end

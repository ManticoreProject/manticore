(* cont.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Continuations.
 *)

structure Cont
(*
:
sig

type 'a cont
val callcc : ('a cont -> 'a) -> 'a
val throw : 'a cont * 'a -> 'b

end
*)
=
struct

    _primcode (
        typedef cont_rep = cont(any);

        define @callcc_impl (f: fun (cont_rep / exh -> any) / exh : exh) : any =
           cont k (x : any) = return (x)
           apply f (k / exh)
        ;

        define @throw_impl (arg: [cont_rep, any] / exh : exh) : any =
           let k : cont_rep = #0(arg)
           let x : any = #1(arg)
           throw k (x)
        ;

    )

type 'a cont = _prim(cont_rep)

val callcc : ('a cont -> 'a) -> 'a = _prim(@callcc_impl)
val throw : 'a cont * 'a -> 'b = _prim(@throw_impl)

end

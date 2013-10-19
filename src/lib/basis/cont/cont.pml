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

        define inline @callcc_impl (callccFun: fun (cont_rep / exh -> any) / exh : exh) : any =
           cont currentContinuation (ccArg : any) = return (ccArg)
           apply callccFun (currentContinuation / exh)
        ;

        define inline @throw_impl (arg: [cont_rep, any] / exh : exh) : any =
           let x : any = #1(arg)
           let k : cont_rep = #0(arg)
           throw k (x)
        ;

    )

type 'a cont = _prim(cont_rep)

val callcc : ('a cont -> 'a) -> 'a = _prim(@callcc_impl)
val throw : 'a cont * 'a -> 'b = _prim(@throw_impl)

end

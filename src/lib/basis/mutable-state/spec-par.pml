(* spec-par.pml
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * support for speculative parallelism that provides runtime support for 
 * rolling back ivars in the event an exception is raised
 *)



structure SpecPar (*: sig
    val spec : (() -> 'a * () -> 'b) -> ('a, 'b)
    end*) = struct

    _primcode(
        define @pSpec(arg : [fun(unit / exh -> any), fun(unit / exh -> any)] / exh:exh):[any,any] = 
            let a : fun(unit / exh -> any) = #0(arg)
            let b : fun(unit / exh -> any) = #1(arg)
            let dummy : any = enum(0) : any
            let res : ![any,any] = alloc(dummy, dummy)
            cont slowClone(_ : unit) = (*work that can potentially be stolen*)
                let res : ![any,any] = promote(res)
                let v_1 : any = apply b(UNIT / exh)
                let v'_1 : any = promote(v_1)
                do #1(res) := v'_1
                return(res) 
            (*24th byte offset gives the current fiber's local storage*)
            let fls : [int,any,int,any,![enum(1)]] = vpload(24, host_vproc) 
            let ite:[any,any] = if NotEqual(#1(fls), UNIT)
                                then let t : [any,any] = ([any,any]) #1(fls)
                                     return(#0(t))
                                else let e : exn = Fail(@"non existent ITE")
                                     throw exh(e)
            let stack:any = #0(ite) (*work group stack*)
            let con_NONE:enum(0) = enum(0):enum(0) (*Option.NONE*)
            let ite:[any,any] = alloc(stack, con_NONE)
            let group:[any,any] = if NotEqual(stack, UNIT)  (*work group*)
                            then let stack:[[any,any],any] = ([[any,any],any]) stack
                                 return (#0(stack))
                            else let e : exn = Fail(@"non existent work group stack")
                                 throw exh(e) 
            let spawnFn:fun([cont(enum(0)),[any,any]] / cont(any) -> enum(0)) = 
                   (fun([cont(enum(0)), [any,any]] / cont(any) -> enum(0))) #1(group) 
            let t:[cont(enum(0)), [any,any]] = alloc(slowClone, ite)                                
            let _:unit = apply spawnFn(t / exh)
            let v_0:any = apply a(UNIT / exh)
            let group:[any,any,any,any] = ([any,any,any,any])group
            let removeFn:fun([cont(enum(0)),[any,any]]/cont(any) -> any) = #3(group)
            let removed:any = apply removeFn(t / exh)
            let notStolen:bool = if NotEqual(removed, UNIT)
                                 then return(true)
                                 else return(false)
            case notStolen
                of true => let v_1:any = apply b(UNIT / exh)
                           let res:[any,any] = alloc(v_0, v_1)
                           return(res)
                | false => let res:![any,any] = promote(res)
                           let v'_0 : any = promote(v_0)
                           do #0(res) := v'_0
                           return(res)
            end
        ;
        
    )

    val spec : ((unit -> 'a) * (unit -> 'b)) -> ('a * 'b) = _prim(@pSpec)


end


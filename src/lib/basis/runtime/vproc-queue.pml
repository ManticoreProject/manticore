#define Q_EMPTY_B enum(0):queue

structure VProcQueue =
  struct


    structure FLS = FiberLocalStorage
    structure PT = PrimTypes

    type data = _prim ( [FLS.fls, PT.fiber, any] )
    datatype queue
      = Q_EMPTY
      | Q_ITEM of data

    _primcode(
define @reverse-queue (fls : FLS.fls, k : PT.fiber, lst : queue, rest : queue / exh : PT.exh) : queue =
  fun revQueue (fls : FLS.fls, k : PT.fiber, lst : queue, acc : queue / exh : PT.exh) : queue =
       let acc : queue = Q_ITEM (alloc(fls, k, acc))
        case lst of 
            Q_EMPTY_B => return (acc)
          | Q_ITEM (item:[FLS.fls, PT.fiber, queue]) =>
             apply revQueue (#0(item), #1(item), #2(item), acc / exh)
        end
  let qitem : queue = apply revQueue (fls, k, lst, rest / exh)
  return (qitem)
;
    )

val _ = print "queue\n"

  end

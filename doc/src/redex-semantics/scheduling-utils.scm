(module scheduling-utils mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 4 4))
           (planet "gui.ss" ("robby" "redex.plt" 4 4))
           (planet "subst.ss" ("robby" "redex.plt" 4 4))
           "scheduling-framework.scm"
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "match.ss"))
  
  (provide lift set-bang init-top-level-schedulers round-robin)
  
  ; emulate set-bang using compare-and-swap
  (define (set-bang l v)
    (term (cas ,l (deref ,l) ,v)))
  
  (define true 1)
  (define false 0)
  
  ;;; spin locks
  
  (define spin-lock-new
    (term
     (ref ,false)))
     
  (define spin-lock
    (term
     (λ (l)
       (fun (loop)
            (if (cas (l ,false ,true))
                (loop)
                (unit))
            (begin 
              (mask-preemption)
              (loop))))))
  
  (define spin-unlock
    (term
     (λ (l)
       (begin 
         ,(set-bang (term l) true)
         (unmask-preemption)))))
  
  ; top-level round-robin scheduler
  (define round-robin
    (term
     (fun (rr sig)
          (handle sig
                  (stop-handler (λ () (run rr (deq-vp))))
                  (preempt-handler (λ (k) (begin
                                            (enq-on-vp (host-vp) k)
                                            (run rr (deq-vp))))))
          rr)))
  
  ; fiber : e -> e
  ; takes an expression for a computation and returns an expression for the fiber that performs the computation.
  (define (fiber f)
    (term
     (λ ()
       (begin
         (,f)
         (forward (stop))))))
  
  ;init-top-level-scheduler : e * e -> e
  ; given a scheduler and an expression, create an expression that runs the source expression under the scheduler
  (define init-top-level-scheduler
    (λ (sched)
      (λ (e)
        (term (run ,sched ,(fiber (term (λ () ,e))))))))
    
  ; init-top-level-schedulers : e * listof(es) -> MP
  ; given an expression for the top level scheduler and a list of seeding expressions, produce an initial
  ; multiprocessor machine state.
  (define (init-top-level-schedulers sched es)
    (make-multiprocessor (map (init-top-level-scheduler sched) es)))
  
  (define (lift op)
    (λ args
      (term
       (ffi-peek (ffi-call ,op ,@args)))))
  
  (define (lt x y)
    (cond
      ((< x y) 1)
      (else 0)))
               
  (define (fib e)
    (term
     (fun (fib n)
          (if ,((lift 'lt) (term n) (term 2))
              n
              ,((lift '+) (term (fib ,((lift '-) (term n) (term 1))))
                          (term (fib ,((lift '-) (term n) (term 2))))))
          ,e)))
  
  (define (fib1 n)
    (fib (term (fib ,n))))
  
  (define (deq-empty)
    (let ([deq '()])
      (let ([deq-push-tl (λ (elt)
                           (set! deq (cons elt deq)))]
            [deq-pop-tl (λ ()
                          (if (null? deq)
                              0
                              (let ([hd (car deq)])
                                (set! deq (cdr deq))
                                hd)))]
            [deq-pop-hd (λ ()
                          (match deq
                            (`() 0)
                            (`(,@(deq1 ...) ,elt) (begin
                                                    (set! deq deq1)
                                                    elt))))])
        (list deq-push-tl deq-pop-tl deq-pop-hd))))
  
  )
(module scheduling-utils mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 4 4))
           (planet "gui.ss" ("robby" "redex.plt" 4 4))
           (planet "subst.ss" ("robby" "redex.plt" 4 4))
           "scheduling-framework.scm"
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "plt-match.ss"))
  
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
     (letcont rr sig
              (fun (dispatch)
                   (run rr (deq-vp))
                   (handle sig
                           (stop-handler (λ () (dispatch)))
                           (preempt-handler (λ (k) (begin
                                                     (enq-on-vp (host-vp) k)
                                                     (dispatch)))))))))
  
  ; fiber : e -> e
  ; takes an expression for a computation and returns an expression for the fiber that performs the computation.
  (define (fiber f)
    (term
     (begin
       (,f)
       (forward (stop)))))
  
  ;init-top-level-scheduler : e * e -> e
  ; given a scheduler and an expression, create an expression that runs the source expression under the scheduler
  (define (init-top-level-scheduler sched e)
    (term (run ,sched ,(fiber (term (λ () ,e))))))
    
  ; init-top-level-schedulers : e * listof(es) -> MP
  ; given an expression for the top level scheduler and a list of seeding expressions, produce an initial
  ; multiprocessor machine state.
  (define (init-top-level-schedulers sched es)
    (make-multiprocessor (map init-top-level-scheduler es)))
      
                              
  )
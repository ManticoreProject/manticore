;;; schedulers-utils-pltr.scm
;;;
;;; COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
;;; All rights reserved.
;;;
;;; Common functions for programming schedulers.

(module scheduler-utils-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 26)))
  (require "schedulers-pltr.scm")
  (require (lib "list.ss")
           (lib "pretty.ss")
           (lib "plt-match.ss"))
  
  (provide set-bang faa fai fiber fiber-store-ans yield atomic-yield dispatch-on schedule-fiber migrate default-action)
  
  ; use cas to achieve set!
  (define (set-bang l v)
    (term (cas ,l (deref ,l) ,v)))
  
  ;; atomic fetch-and-add defined in terms of cas
  (define (faa l x)
    (term (fun (loop)
               (let ((y (deref ,l)))
                 (if (=i y (cas ,l y (+ y ,x)))
                     y
                     (loop)))
               (loop))))

  ;; atomic fetch-and-increment
  (define (fai l)
    (faa l (term 1)))
  
  ; Fiber creates a new fiber that runs the term f : any -> any. Before finishing,
  ; set the store location 0 to the return value of f.
  (define (fiber-store-ans f)
    (term
     (λ () (begin
             ,(set-bang (term 0) (term (,f)))   ;; run f and set location 0 to its result
             (forward (stop))))))
  
  (define (fiber f)
    (term
     (λ () (begin
             (,f) ;; just run f
             (forward (stop))))))

  ; Yield control of the vproc, but first pass the current continuation for
  ; resumption.
  (define yield
    (term
     (letcont k (unit) 
              (forward (preempt k)))))
  
  ; Mask signals before resuming.
  (define atomic-yield
    (term
     (begin
       ,yield
       (mask-preemption))))
  
  ; Make a fiber for the function f, and schedule it under the scheduler action act.
  (define (schedule-fiber act f)
    (fiber (term (λ () (begin (mask-preemption) (run ,act ,(fiber f)))))))

  ; Run the scheduler action act on virtual processor vp.
  (define (dispatch-on vp act)
    (term
     (enq-on-vp ,vp ,(schedule-fiber act (term (λ () (forward (stop))))))))
  
  ; Migrate the running fiber to the vproc vp.
  (define (migrate vp)
    (term
     (letcont k (unit)
              (begin (enq-on-vp ,vp k)
                     (forward (stop))))))
  
  (define default-action
    (term 
     (fun (act sig)
          (handle sig
                  (stop-handler 
                   (λ () (run act (deq-vp))))
                  (preempt-handler 
                   (λ (k) (begin
                            (enq-on-vp (host-vp) k)
                            (run act (deq-vp))))))
       act)))
  
  ; The default, top-level scheduler action.  This action implements round-robin
  ; scheduling.
  (define default-action2
    (term 
     (letcont act sig
              (handle sig
                      (stop-handler 
                       (λ () (run act (deq-vp))))
                      (preempt-handler 
                       (λ (k) (begin
                                (enq-on-vp (host-vp) k)
                                (run act (deq-vp))))))
       act)))
  )

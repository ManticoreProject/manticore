;;; schedulers-utils-pltr.scm
;;;
;;; COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
;;; All rights reserved.
;;;
;;; Common functions for programming schedulers.

(module scheduler-utils-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 11))
           (planet "random.ss" ("schematics" "random.plt" 1 0))
           (planet "gui.ss" ("robby" "redex.plt" 3 11)))
  (require "schedulers-pltr.scm")
  (require (lib "list.ss")
           (lib "pretty.ss")
           (lib "plt-match.ss"))
  
  (provide set-bang fiber yield atomic-yield dispatch-on schedule-fiber migrate)
  
  ; use cas to achieve set!
  (define (set-bang l v)
    (term (cas ,l (deref ,l) ,v)))
  
  ; Fiber creates a new fiber that runs the term f : any -> any. Before finishing,
  ; set the store location 0 to the return value of f.
  (define (fiber f)
    (term-let ((x-new (variable-not-in f (term xarg))))
              (term
               (λ (x-new) (begin
                            ,(set-bang (term 0) (term (,f (unit))))   ;; run f and set location 0 to its result
                            (forward (stop)))))))

  ; Yield control of the vproc, but pass the current continuation for
  ; resumption.
  (define yield
    (term
     (letcont k x (unit) (forward (preempt k)))))
  
  ; Mask signals before resuming.
  (define atomic-yield
    (term
     (begin
       ,yield
       (mask-preemption))))
  
  ; Make a fiber for the function f, and schedule it under the scheduler action act.
  (define (schedule-fiber act f)
    (fiber (term (λ (x) (run ,act ,(fiber f))))))

  ; Run the scheduler action act on virtual processor vp.
  (define (dispatch-on vp act)
    (term
     (enq-on-vp vp ,(schedule-fiber act (term (λ (x) (forward (stop))))))))
  
  ; Migrate the running fiber to the vproc vp.
  (define (migrate vp)
    (term
     (letcont k x (unit)
              (begin (enq-on-vp ,vp k)
                     (forward (stop))))))
  )

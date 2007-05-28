;;; asynchronous-signals-pltr.scm
;;;
;;; COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
;;; All rights reserved.
;;;
;;; Prototype for asynchronous signals.

(module asynchronous-signals-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 11))
           (planet "random.ss" ("schematics" "random.plt" 1 0))
           (planet "gui.ss" ("robby" "redex.plt" 3 11)))
  (require "schedulers-pltr.scm")
  (require "scheduler-utils-pltr.scm")
  (require (lib "list.ss")
           (lib "pretty.ss")
           (lib "plt-match.ss"))
  
  (provide killable-action)

  (define killable-action
    (term
     (λ (term-flag)
       (letrec ((act (λ (sign)
                       (handle sign
                               (stop-handler (λ (xunit) (forward (stop))))
                               (preempt-handler 
                                (λ (k) (begin
                                         (letcont k-a x (unit) (forward (preempt k-a)))
                                         (if0 (deref term-flag) (forward (stop)) (run act k)))))))))
         act))))
  
  )

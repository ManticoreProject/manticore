;;; work-stealing-scheduler-pltr.scm
;;;
;;; COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
;;; All rights reserved.
;;;
;;; Prototype work-stealing scheduler.

(module work-stealing-scheduler-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 13))
           (planet "random.ss" ("schematics" "random.plt" 1 0)))
  (require "schedulers-pltr.scm")
  (require "scheduler-utils-pltr.scm")
  (require (lib "list.ss")
           (lib "pretty.ss")
           (lib "plt-match.ss"))
  
  (provide work-stealing)
  
  (define (work-stealing f)
    (term
     (let ((vp (provision (gid))))
       (let ((q1 (ref (queue))))
         (let ((q2 (ref (queue))))
           (let ((ws-action 
                  (λ (my-q) 
                    (λ (other-q)
                      (letrec ((ws-switch (λ (sign)
                                            (letrec ((new-work (λ (nq1) 
                                                                 (λ (nq2)
                                                                   (let ((k-opt (deq nq1)))
                                                                     (if0 k-opt
                                                                          ((new-work nq2) nq1)
                                                                          (run ws-switch k-opt)))))))
                                              (handle sign
                                                      (stop-handler (λ (x) ((new-work my-q) other-q)))
                                                      (preempt-handler 
                                                       (λ (k) (begin
                                                                (enq my-q k)
                                                                ,atomic-yield
                                                                ((new-work my-q) other-q)))))))))
                        ws-switch)))))
             (begin
               ,(dispatch-on (term vp) (term ((ws-action q2) q1)))
               (run ((ws-action q1) q2) ,(fiber (term (λ (x) (f q1))))))))))))
  
  )
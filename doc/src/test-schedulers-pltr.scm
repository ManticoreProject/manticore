(module test-schedulers-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 11))
           (planet "gui.ss" ("robby" "redex.plt" 3 11)))
  (require "schedulers-pltr.scm")
  (require (lib "list.ss")
           (lib "plt-match.ss"))
  
  (define tcas
    (list (term (let ((x (ref 999)))
                  (let ((f (λ (z) (deref x))))
                    (let ((p (provision (gid))))
                      (begin
                        (enq-on-vp p ,(fiber (term f)))
                        (cas x (deref x) 888))))))
          (term ((deq-vp) (unit)))))
  
  (define (r t)
    (run t))
  
  (define (s t)
    (step t))
 
  )
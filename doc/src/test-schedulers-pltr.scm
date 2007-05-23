(module test-schedulers-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 11))
           (planet "gui.ss" ("robby" "redex.plt" 3 11)))
  (require "schedulers-pltr.scm")
  (require (lib "list.ss")
           (lib "plt-match.ss"))
  
  (define (set-bang l v)
    (term (cas ,l (deref ,l) ,v)))
  
  (define t1
    (list (term (+ 1 2))))
  
  (define t2
    (list (term (run 1 2))))
  
  (define tref
    (list (term (let ((x (ref (+ 1 23)))) 
                  (let ((y (ref 9)))
                    (+ (deref x) (deref y)))))
          (term (let ((x (ref 999))) (deref x)))))
  
  
  (define (t3)
    (list (term (+ (+ 500 3) 2)) (term (+ 3 4))))
 
  (define (t4)
    (list (term (+ 4 3)) (term (+ 59 (letcont k x (+ x 1) (+ 2 (k 1)))))))
  
  (define (t5)
    (run (list (term (enq-on-vp 1 43)) (term (let ((g 6)) (provision g))))))
  
  (define (t6)
    (list (term (let ((x (gid))) 
                       (let ((p (provision x))) 
                         (enq-on-vp p 99))))
               (term 888)))
  
  (define (fiber f)
    (term-let ((x_new (variable-not-in f (term x))))
              (term (letcont k x_new (begin 
                                       ;(,f (unit))
                                       ,(set-bang (term 0) (term (,f (unit))))
                                       (forward (stop))) k))))
  
  (define greedy-action
    (term
     (letrec ((act (λ (sig)
                     (handle sig
                             (stop-handler (λ (x) (forward (stop))))
                             (preempt-handler (λ (k) (run act k)))))))
       act)))
                     
  (define default-action
    (term (letrec ((act (λ (sig)
                        (handle sig
                                (stop-handler (λ (x) (run act (deq-vp))))
                                (preempt-handler (λ (k) (begin
                                                          (enq-vp k)
                                                          (run act (deq-vp)))))))))
            act)))
  
  (define t
    (term (λ (x) (let ((k ,(fiber (term (λ (x) 999)))))
                   (enq-vp k)))))
  
  (define (test-sched act e)
    (list (term (run ,act ,(fiber (term (λ (x) ,e)))))))
  
  (define tcas
    (list (term (let ((x (ref 999)))
                  (let ((f (λ (z) (deref x))))
                    (let ((p (provision (gid))))
                      (begin
                        (enq-on-vp p ,(fiber (term f)))
                        ,(set-bang (term x) (term 999)))))))
          (term ((deq-vp) (unit)))))
  
  (define (sum e)
    (term
     (letrec ((sum (λ (n)
                      (if0 n
                           0
                           (+ n (sum (+ n ,(- 1))))))))
       (,e sum))))
  
  (define (tsum n)
     (sum (term (λ (sum)
                  (sum ,n)))))
    
  
  (define (r t)
    (run t))
  
  (define (s t)
    (step t))
  
  )
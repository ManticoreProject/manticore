;;; por-pltr.scm
;;;
;;; COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
;;; All rights reserved.
;;;
;;; Prototype for the parallel-or operator.

(module por-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 13))
           (planet "random.ss" ("schematics" "random.plt" 1 0)))
  (require "schedulers-pltr.scm")
  (require "asynchronous-signals-pltr.scm")
  (require "scheduler-utils-pltr.scm")
  (require (lib "list.ss")
           (lib "pretty.ss")
           (lib "plt-match.ss"))
  
  (provide por full empty-c done mark-full mark-empty)
  
  (define true-c (term 0))
  (define false-c (term 1))
  
  (define (kill flg vp)
    (term
     (begin ,(set-bang flg true-c)
            (signal-vp ,vp))))
  
  (define full (term 111))
  (define empty-c (term 222))
  (define done (term 333))
  
  (define (orelse e1 e2) (term (if0 ,e1 ,true-c ,e2)))
  
  (define (cas-b l ov nv)
    (term
     (=i ,ov (cas ,l ,ov, nv))))
  
  (define (mark-full c)
    (term
     (if0 ,(orelse (cas-b c empty-c full)
                   (cas-b c done full))
          (unit)
          (forward (stop)))))
  
  (define (mark-empty c)
    (term
     (if0 ,(cas-b c empty-c done)
          (forward (stop))
          (if0 (=i (deref ,c) ,full)
               (forward (stop))
               (unit)))))
  
  (define (por-impl vp1 vp2 ret-k)
    (term
     (let ((term-flag (ref ,false-c)))
       (let ((cell (ref ,empty-c)))
         (let ((wrapper 
                (λ (vp)
                  (λ (f)
                    ,(schedule-fiber (term (,killable-action term-flag))
                                     (term 
                                      (λ (x)
                                        (let ((v (f (unit))))
                                               (begin
                                                 (if0 v
                                                      ,(mark-empty (term cell))
                                                      (begin ,(mark-full (term cell))
                                                             ,(kill (term term-flag) (term vp))))
                                                 (,ret-k v))))))))))
           (λ (f1) (λ (f2)
                     (let ((w1 ((wrapper ,vp2) f1)))
                       (let ((w2 ((wrapper ,vp1) f2)))
                         (begin
                           (enq-vp w1)
                           (enq-on-vp ,vp2 w2)))))))))))
  
  (define (por e1 e2)
    (term
     (let ((f1 (λ (x) ,e1)))
       (let ((f2 (λ (x) ,e2)))
         (let ((vp (provision (gid))))
           (letcont k1 xr xr 
                    ((,(por-impl (term 0) (term vp) (term k1))
                      f1)
                     f2)))))))
  )
;;; test-schedulers-pltr.scm
;;;
;;; COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
;;; All rights reserved.
;;;
;;; Test code for the Manticore semantics.

(module test-schedulers-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 11))
           (planet "random.ss" ("schematics" "random.plt" 1 0))
           (planet "gui.ss" ("robby" "redex.plt" 3 11)))
  (require "schedulers-pltr.scm")
  (require "work-stealing-scheduler-pltr.scm")
  (require "asynchronous-signals-pltr.scm")
  (require "scheduler-utils-pltr.scm")
  (require "por-pltr.scm")
  (require (lib "list.ss")
           (lib "pretty.ss")
           (lib "plt-match.ss"))
  
  ; Similar to apply-reduction-relation*, except this function drops circular reduction 
  ; sequences.
  (define (reduction* max-reducts reductions exp)
    (let ([answers (make-hash-table 'equal)]
          [visited (make-hash-table 'equal)]
          [d 0])
      (let loop ([exp exp])
        (let ([nexts (apply-reduction-relation reductions exp)]
              [nexts2 (apply-reduction-relation/tag-with-names reductions exp)])
          (let ([uniq (mk-uniq nexts)]
                [visited? (lambda (exp) (hash-table-get visited exp #f))])
            (unless (= (length uniq) (length nexts))
              (error 'apply-reduction-relation*
                     "term ~s produced non unique results:~a ~s ~s"
                     exp
                     (apply
                      string-append
                      (map (λ (x) (format "\n~s" x)) nexts2))
                     (length uniq) (length nexts)))
            (unless (< d max-reducts)
              (error 'reduction*
                     "term ~s produced over ~s reductions"
                     exp
                     max-reducts))
            (set! d (add1 d))  
            (unless (visited? exp)
              (begin
                ; mark exp as visited
                (hash-table-put! visited exp #t)
                (cond
                  [(null? uniq) (hash-table-put! answers exp #t)]
                  [else (for-each loop uniq)])))
            )))
      (hash-table-map answers (λ (x y) x))))
  
  (define (pick-random-elt ls)
    (let ([n (length ls)])
      (if (zero? n)
          '()
          (let* ([i (random-integer n)])
            (letrec ([ith-elt 
                      (lambda (ls j)
                        (cond
                          [(null? ls) (error "impossible")]
                          [(= i j) (list (car ls))]
                          [#t (ith-elt (cdr ls) (add1 j))]))])
              (ith-elt ls 0))))))
  
  ; Follow a random path through the reduction tree to a terminal state, dropping
  ; circular reduction sequences.  I avoid such circularity by tracking all visited
  ; states, and attempting a new path when one shows up.
  (define (random-reduction-path max-reducts reductions exp)
    (let ([answers (make-hash-table 'equal)]
          [visited (make-hash-table 'equal)]
          [d 0])
      (let loop ([exp exp])
        (let ([nexts (apply-reduction-relation reductions exp)]
              [nexts2 (apply-reduction-relation/tag-with-names reductions exp)])
          (let ([uniq (mk-uniq nexts)]
                [visited? (lambda (exp) (hash-table-get visited exp #f))])
            (unless (= (length uniq) (length nexts))
              (error 'apply-reduction-relation*
                     "term ~s produced non unique results:~a ~s ~s"
                     exp
                     (apply
                      string-append
                      (map (λ (x) (format "\n~s" x)) nexts2))
                     (length uniq) (length nexts)))
            (unless (< d max-reducts)
              (error 'reduction*
                     "term ~s produced over ~s reductions"
                     exp
                     max-reducts))
            (set! d (add1 d))              
            (begin
              ; mark exp as visited
              (hash-table-put! visited exp #t)
              (cond
                [(null? uniq) (hash-table-put! answers exp #t)]
                [else (for-each loop (pick-random-elt uniq))]))
            )))
      (hash-table-map answers (λ (x y) x))))
  
  ;; mk-uniq : (listof X) -> (listof X)
  ;; returns the uniq elements (according to equal?) in terms.
  (define (mk-uniq terms)
    (let ([ht (make-hash-table 'equal)])
      (for-each (λ (x) (hash-table-put! ht x #t)) terms)
      (hash-table-map ht (λ (k v) k))))
  
  (define t1
    (list (term (+ 1 2))))
  
  (define tbegin
    (list (term ((λ (x) (begin x x)) (unit)))))
  
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
                                (stop-handler (λ (xunit) (run act (deq-vp))))
                                (preempt-handler (λ (k) (begin
                                                          (enq-vp k)
                                                          (run act (deq-vp)))))))))
            act)))
  
  (define t
    (term (λ (x) (let ((k ,(fiber (term (λ (x) 999)))))
                   (enq-vp k)))))
  
  (define (test-sched act es)
    (map (lambda (e) (term (run ,act ,(fiber (term (λ (,(variable-not-in (term e) (term x-unit))) ,e)))))) es))
  
  (define tcas
    (list (term (let ((x (ref 999)))
                  (let ((f (λ (z) (deref x))))
                    (let ((p (provision (gid))))
                      (begin
                        (enq-on-vp p ,(fiber (term f)))
                        ,(set-bang (term x) (term 999)))))))
          (term ((deq-vp) (unit)))))
  
  (define test-threads-1vp
    (list (term 
           (begin
             (enq-vp ,(fiber (term (λ (x) (+ 34 1)))))
             (+ 1 2)))))
  
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

  (define (remove-fix e)
    (match e
      [(? symbol? s) s]
      [(? number? n) n]
      (`(fix ,e) `(fix))
      (es (map remove-fix es))))
  
  (define (r sched ts)
    (parameterize ([initial-char-width 40]
                   [initial-font-size 7]
                   [reduction-steps-cutoff 100])
      (traces scheduler-language multiprocessor-machine (mk-multiprocessor (test-sched sched ts)))))
  
  
  (define (s sched ts)
    (parameterize ([initial-char-width 40]
                   [initial-font-size 7])
      (stepper-with-map scheduler-language multiprocessor-machine (mk-multiprocessor (test-sched sched ts))
                        (lambda (x) (remove-fix x))
                        )))
  
  (define (random-test sched ts n)
    (random-reduction-path n multiprocessor-machine (mk-multiprocessor (test-sched sched ts))))
  
  (define test-signal
    (list (term
           (let ((f (λ (z) (+ 888 1))))
             (let ((p (provision (gid))))
               (begin
                 (enq-on-vp p ,(fiber (term f)))
                 (signal-vp p)))))
          (term ((deq-vp) (unit)))))
    
  (define test-kill-fiber
    (list (term
           (let ((f (λ (z) (+ 888 1))))
             (let ((kc (ref 0)))
               (let ((ka (,killable-action kc)))
                 (let ((p (provision (gid))))
                   (begin
                     (enq-on-vp p ,(schedule-fiber (term ka) (term f)))
                     (signal-vp p)))))))
          (term ((deq-vp) (unit)))))
  
  (define test-kill-action
    (list (term
           (let ((r (ref 0)))
             ((,killable-action r) (preempt (λ (x) 999)))))))
  
  (define test-q
    (list (term
           (let ((q (ref (queue))))
             (begin
               (enq q 999)
               (deq q))))))
    
  (define test-ws-1
    (list 
     (term
      (let ((f (λ (q)
                 (enq q ,(fiber (term (λ (x) (forward (stop)))))))))
        ,(work-stealing (term f))))
     (term ((deq-vp) (unit)))))
  
  (define test-migrate
    (list 
     (term
      (let ((vp (provision (gid))))
        (begin
          ,(migrate (term vp))
          (+ 999 1))))
     (term ((deq-vp) (unit)))))
  
  (define test-por-cell
    (list (term
           (let ((c (ref ,done)))
             ,(mark-full (term c))))))
  
  (define test-por
    (list (por (term 999) (term 0))
          (term ((deq-vp) (unit)))))

  )
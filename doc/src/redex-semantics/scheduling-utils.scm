(module scheduling-utils mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 4 4))
           (planet "gui.ss" ("robby" "redex.plt" 4 4))
           (planet "subst.ss" ("robby" "redex.plt" 4 4))
           "scheduling-framework.scm"
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "match.ss"))
  
  (provide lift set-bang init-top-level-schedulers round-robin fiber terms-equal? new-cancelable wait-for-inactive set-in-cancelable get-from-cancelable is-nil? get-val remove-dict set-val set-inactive get-parent wrap cancel atomic-yield-op yield-op)
  
  (define (lift op)
    (λ args
      (term
       (ffi-peek (ffi-call ,op ,@args)))))

  ; emulate set-bang using compare-and-swap
  (define (set-bang l v)
    (term 
     (fun (lp)
          (let ((v0 (deref ,l)))
            (if ,((lift 'terms-equal?) (term (cas ,l (deref ,l) ,v)) (term v0))
                (unit)
                (lp)))
          (lp))))
  
  (define test-set-bang
     (set-bang 0 1))
  
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
  
  (define yield-op
    (term
     (letcont k (unit)
              (forward (preempt k)))))
  
  (define atomic-yield-op
    (term
     (begin ,yield (mask-preemption))))
  

  (define (lt x y)
    (cond
      ((< x y) 1)
      (else 0)))
  
  ; our "unit" constant counts as a nil value
  (define (is-nil? x)
    (match x
      (`(unit) 1)
      (_ 0)))
  
  ; structural equality test for terms
  (define (terms-equal? x y)
    (if (equal? x y)
        1
        0))
  
  (define nil (term (unit)))
               
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
  
  (define (get-val v xs)
    (letrec ([get
              (lambda (xs)
                (if (null? xs)
                    (error 'get-val)
                    (if (equal? (caar xs) v)
                        (cdar xs)
                        (get (cdr xs)))))])
      (get xs)))
  
  (define (set-val k v x)
    (cons (cons k v) x))
  
  (define (get-parent c)
    ((lift 'get-val) "parent" (term (deref ,c))))
  
  (define (add-child c child)
    (cons (cons "child" child) c))
  
  (define (add-parent parent child)
    (term
     (ffi-call ,'set-val "parent" ,parent ,child)))
  
  (define current-c
    (term (get-from-fls (get-fls) "current-cancelable")))
  
  (define (new-c-with-parent parent)
    (let ((fields (list (cons "canceled" 0) (cons "inactive" 1))))
      (let ((fields-term (term (ffi-val ,fields))))
        (if parent
            (add-parent parent fields-term)
            fields-term))))
  
  (define new-c-no-parent
    (new-c-with-parent 0))
  
  (define (add-to-children parent child)
    (term
     (let ((pstruct (ffi-call ,'add-child (deref ,parent) ,child)))
       ,(set-bang parent (term pstruct)))))
  
  ; create a cancelable
  (define new-cancelable
    (term
     (let ((parent ,current-c))
       (if ,((lift 'is-nil?) (term (deref parent)))
           (let ((new (ref ,(new-c-with-parent 0))))
             new)
           (let ((new (ref ,(new-c-with-parent (term parent)))))
             (begin 
               ,(add-to-children (term parent) (term new))
               new))))))
  
  ; set the current cancelable
  (define (set-current c)
    (term
     ,(set-bang current-c c)))
  
  (define test-new
    (term
     (begin
       ,current-c
       ,(set-current new-c-no-parent)
       ,new-cancelable)))
  
  ;; remove-dict : e -> listof(e*d) -> listof(e*d)
  ;; remove any elements from the dictionary d with the key y
  (define (remove-dict y d)
    (filter (lambda (x) (not (equal? (car x) y))) d))
  
  (define (set-current-cancelable c)
    (term
     (set-in-fls (get-fls) "current-cancelable" ,c)))
  
  (define test-cc
    (set-current-cancelable test-new))
  
  ;; set-in-cancelable : e -> string -> any -> e
  ;; update a value in the cancelable structure. multiple fibers can perform this operation in parallel. so,
  ;; to make the operation concurrent, we use a transaction protocol.
  (define (set-in-cancelable c fld v)
    (term
     (fun (si-lp)
          (let ((cv-init (deref ,c)))
            (let ((cv (ffi-call ,'remove-dict ,fld cv-init)))
              (let ((x (ffi-call ,'cons ,fld ,v)))
                (let ((cv (ffi-call ,'cons x cv)))
                  (if ,((lift 'terms-equal?) (term (cas ,c cv-init cv)) (term cv-init))
                      (unit)
                      (si-lp))))))
          (si-lp))))
  
  
  (define (get-from-cancelable c fld)
    ((lift 'get-val) fld (term (deref ,c))))
  
  (define test-sic
    (term
     (let ((c ,test-new))
       (begin
         ,(set-in-cancelable (term c) "canceled" 1)
         (if ,(get-from-cancelable (term c) "canceled")
             (unit)
             (ffi-call ,'error "canceled"))))))
       
  (define (set-inactive c)
    (term
     (let ((parent ,(get-parent c)))
       (begin
         ,(set-in-cancelable c "inactive" 1)
         ,(set-current-cancelable (term parent))))))
  
  (define test-inactive
    (term
     (let ((c ,test-new))
       ,(set-inactive (term c)))))
  
  (define (set-active c)
    (term
     (begin
       ,(set-current-cancelable c)
       ,(set-in-cancelable (term c) "inactive" 0))))
  
  (define (wait-for-inactive c)
    (term
     (fun (lp)
          (if ,(get-from-cancelable c "inactive")
              (unit)
              (lp))
          (lp))))
  
  (define wff-ex
     (wait-for-inactive new-cancelable))

  (define cancel
    (term
     (λ (c)
       (begin
         ,(set-in-cancelable (term c) "canceled" 1)
         (if ,(get-from-cancelable (term c) "canceled")
             (unit)
             (ffi-call ,'error "canceled"))
         ,(wait-for-inactive (term c))))))
  
  (define test-cancel
    (term 
     (let ((c ,test-new))
       (begin
         (,cancel c)
         (if ,(get-from-cancelable (term c) "canceled")
             (unit)
             (ffi-call ,'error "canceled"))))))
  
  ; terminate the fiber associated with the cancelable
  (define (terminate c)
    (term
     (begin
;       ,(app-children c cancel)
       ,(set-inactive c)
       (ffi-call ,'printf  "terminated~s~n~n" (deref c))
       (forward (stop)))))

  ; run the cancelable fiber
  (define (dispatch c wrap k)
    (term
     (if ,(get-from-cancelable (term c) "canceled")
         ,(terminate c)
         (begin ,(set-active c) (ffi-call ,'printf  "dispatch~s~n~n" (deref c)) (run ,wrap ,k)))))
  
  ; scheduler action to make the fiber cancelable
  (define (wrap c k)
    (term
     (fun (wrap-act sig)
          (handle sig
                  (stop-handler (λ () ,(terminate c)))
                  (preempt-handler 
                   (λ (k) 
                     (begin
                       ,(set-inactive c)
;                       ,atomic-yield                       
                       ,(dispatch c (term wrap-act) (term k))))))
                   (λ () ,(dispatch c (term wrap-act) k)))))
  
  (define (is-null ls)
    (if (null? ls) 1 0))
  
  (define (app f ls)
    (term
     (fun (lp ls)
          (if ,((lift 'is-null) (term ls))
              (unit)
              (begin
                (,f (ffi-call ,'car ls))
                (lp (ffi-call ,'cdr ls))))
          (lp ,ls))))
  
  (define (filt-children c)
    (map cdr (filter (lambda (c-ent) (equal? (car c-ent) "child")) c)))
  
  (define (get-children c)
    (term
     (ffi-call ,'filt-children (deref ,c))))
  
  (define (app-children f c)
    (app f (get-children c)))
  
  (define test-app-children
    (term
     (let ((c (ref (ffi-val (,(cons "x" 1) ,(cons "child" 0) ,(cons "child" 123))))))
       (fun (f x) (ffi-call ,'print x)
            ,(app-children (term f) (term c))))))
  
  (define test-app
    (term
     (let ((ls (ffi-val ,(list 1 2))))
       (fun (f x) (ffi-call ,'print x)
            ,(app (term f) (term ls))))))
  
                
  
  )
(module scheduling-utils mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 4 4))
           (planet "gui.ss" ("robby" "redex.plt" 4 4))
           (planet "subst.ss" ("robby" "redex.plt" 4 4))
           "scheduling-framework.scm"
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "match.ss"))
  
  (provide lift set-bang init-top-level-schedulers round-robin fiber terms-equal? new-cancelable wait-for-inactive set-in-cancelable is-nil? get-val remove-dict set-val)
  
  ; emulate set-bang using compare-and-swap
  (define (set-bang l v)
    (term (let ((v1 ,v))
            (let ((l1 ,l))
              (cas l1 (deref l1) v1)))))
  
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
  
  (define atomic-yield
    (term
     (letcont k x (mark-preemption)
              (forward (preempt k)))))
  
  (define (lift op)
    (λ args
      (term
       (ffi-peek (ffi-call ,op ,@args)))))
  
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
  
  (define cancelable-tag 0)
  
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
  ;; update a value in the cancelable structure
  (define (set-in-cancelable c fld v)
    (term
     (let ((cv (deref ,c)))
       (let ((cv (ffi-call ,'remove-dict ,fld cv)))
         (let ((x (ffi-call ,'cons ,fld ,v)))
           (let ((cv (ffi-call ,'cons x cv)))
             ,(set-bang c (term cv))))))))
  
  
  (define (get-from-cancelable c fld)
    ((lift 'get-val) fld (term (deref ,c))))
  
  (define test-sic
    (term
     (let ((c ,test-new))
       ,(set-in-cancelable (term c) "inactive" 0))))
       
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
       ,(set-current-cancelable c))))
  
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
     (fun (cancel c)
          (begin
            ,(set-in-cancelable (term c) "canceled" 1)
            ,(wait-for-inactive (term c)))
          cancel)))
  
  (define (wrap c k)
    (term
     (fun (term)
         (begin
           ,(app-children (term c) cancel)
           ,(set-inactive c)
           (forward (stop)))
         (fun (dispatch wrap k)
              (if ,(get-from-cancelable c "canceled")
                  (term)
                  (begin ,(set-active c) (run wrap k)))
              (fun (wrap-act sig)
                   (handle sig
                           (stop-handler (λ () term))
                           (preempt-handler 
                            (λ (k) 
                              (begin
                                ,set-inactive
                                ,atomic-yield
                                (dispatch wrap-act k)))))
                   (letcont kwrapped x (dispatch wrap-act ,k)
                            kwrapped))))))
  
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
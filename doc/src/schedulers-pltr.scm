;;; schedulers-pltr.scm
;;;
;;; COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
;;; All rights reserved.
;;;
;;; This module contains a PLT Redex model of the Manticore semantics, particularly those
;;; operations involved in scheduling.

(module schedulers-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 11))
           (planet "gui.ss" ("robby" "redex.plt" 3 11)))
  (require (lib "list.ss")
           (lib "plt-match.ss"))
  
  (define scheduler-language
    (language 
     ; multiprocessor state (contains each vproc in the multiprocessor)
     (MP ((vps VP ...) global-store provision-map))
     ; provisioning map from a unique ID (the gid) to a set of vproc IDs
     (provision-map (pmap (number number ...) ...))
     ; store
     (global-store (store (x v) ...))
     ; virtual processor state (scheduler action stack, thread queue, mask bit, expression)
     (VP (vproc number action fiber-queue mask e))
     ; scheduler-action stack
     (action (action-stack v ...))
     ; fiber queue
     (fiber-queue (queue v ...))
     ; mask bit
     (mask (masked) (unmasked))
     ; scheduler signals
     (signal (stop) (preempt e))
     ; values
     (v number (λ (x) e) (unit) signal)
     ; expressions
     (e 
      ; core expressions
      (e e) (+ e e) (if0 e e e) x v (begin e e ...) (let ((x e)) e) (letrec ((x e)) e) (fix e)
      ; first-class continuations
      (abort e) (letcont x x e e)
      ; scheduler operations
      (run e e) (forward e)
      ; fiber queue operations
      (deq e) (enq e e) (deq-vp) (enq-vp e) (enq-on-vp e e)
      ; allocating / deallocating vprocs
      (gid) (provision e) (release e)
      ; atomic operations
      (cas e e e) (ref e) (deref e)
      ; signal handler
      (handle e (stop-handler e) (preempt-handler e)))
      ; contexts
      (E (E e) (v E) (+ E e) (+ v E) (if0 E e e) (begin E e e ...) (run E e) (run v E)
         (enq E e) (enq v E) (enq-vp E) (enq-on-vp E e) (enq-on-vp v E) (deq E) (forward E)
         (provision E) (release E)
         (case E e e) (cas v E e) (cas v v E) (ref E) (deref E)
         (let ((x E)) e) (handle E (stop-handler e) (preempt-handler e)) hole)
     ; keywords
     (x (variable-except λ + abort letcont begin unit if0 let letrec fix
                         forward run stop preempt handle enq deq enq-vp deq-vp enq-on-vp
                         gid provision release
                         cas ref deref
                         in out queue action-stack store pmap vps vproc
                         preempt-handler stop-handler))))
  
  ;; contains : number * listof(number) -> bool
  ;;
  ;; does the list ls contain the number n?
  (define (contains n ls)
    (cond ((null? ls) #f)
          ((= n (car ls)) #f)
          (#t (contains n (cdr ls))))) 
  
  ;; provision : vp-num * listof(vproc) * listof(vp-num) -> vp-num
  ;;
  ;; provision returns either the first vproc not in the provisioned set
  ;; or the host vproc
  (define (provision host-vp vps provisioned)
    (let ([find
           (lambda (vp vps)
             (match vp
                 (`(vproc ,n ,s ,q ,m ,e) (if (contains n provisioned)
                                              vps
                                              (cons n vps)))))])
      (car (foldl find (list host-vp) vps))))
  
  ;;; gid : listof(number * listof(number)) -> number
  ;;;
  ;;; return a unique group id w.r.t the provisioning map pmap
  (define (gid pmap)
    (letrec ((f (lambda (max pmap)
                  (cond ((null? pmap) max)
                        ((< (caar pmap) max) (f max (cdr pmap)))
                        (#t (f (caar pmap) (cdr pmap)))))))
      (add1 (f 0 pmap))))
        
  (define multiprocessor-machine
    (reduction-relation 
     scheduler-language
     
     ;; sequential transitions
     (s==> (in-hole E_1 (+ number_1 number_2))
           (in-hole E_1 ,(+ (term number_1) (term number_2)))
           "+")
     
     (s==> (in-hole E_1 ((λ (x_1) e_1) v_1))
           (in-hole E_1 (subst (x_1 v_1 e_1)))
           "βv")
     
     (s==> (in-hole E_1 (fix (λ (x_1) e_1)))
           (in-hole E_1 (subst (x_1 (fix (λ (x_1) e_1)) e_1)))
           "fix")
     
     (s==> (in-hole E_1 (abort e_1))
           e_1
           "abort")
     
     ;name capture bug???
     (s==> (in-hole E_1 (letcont x_1 x_2 e_1 e_2))
           (in-hole E_1 (letrec ((x_1 (λ (x_2) (abort (in-hole E_1 e_1)))))
                          e_2))
           "letcont")

     (s==> (in-hole E_1 (let ((x_1 v_1)) e_2))
           (in-hole E_1 (subst (x_1 v_1 e_2)))
           "let")
     
     (s==> (in-hole E_1 (letrec ((x_1 e_1)) e_2))
           (in-hole E_1 (let ((x_1 (fix (λ (x_1) e_1)))) e_2))
           "letrec")
     
     (s==> (in-hole E_1 (if0 0 e_1 e_2))
           (in-hole E_1 e_1)
           "if0-t")
     
     (s==> (in-hole E_1 (if0 number_1 e_1 e_2))
           (in-hole E_1 e_2)
           (side-condition (not (= 0 (term number_1))))
           "if0-f")
     
     (s==> (in-hole E_1 (begin v e_1 e_2 ...))
           (in-hole E_1 (begin e_1 e_2 ...))
           "begin-many")
     
     (s==> (in-hole E_1 (begin e_1))
           (in-hole E_1 e_1)
           "begin-one")
     
     (s==> (in-hole E_1 (handle (stop)
                                (stop-handler e_s)
                                (preempt-handler e_k)))
           (in-hole E_1 e_s)
           "handle-stop")
     
     (s==> (in-hole E_1 (handle (preempt v_k)
                                (stop-handler e_s)
                                (preempt-handler e_k)))
           (in-hole E_1 (e_k v_k))
           "handle-preempt")
     
     ;; vproc transitions
     (vp=> (vproc number_1 (action-stack v_a1 ...) fiber-queue_1 mask_1 (in-hole E_1 (run v_a2 v_k)))
           (vproc number_1 (action-stack v_a2 v_a1 ...) fiber-queue_1 (unmasked) (v_k (unit)))
           "run")
     
     (vp=> (vproc number_1 (action-stack v_a v_acts ...) fiber-queue_1 mask_1 (in-hole E_1 (forward v_sig)))
           (vproc number_1 (action-stack v_acts ...) fiber-queue_1 mask_1 (v_a v_sig))
           "forward")
     
;     (vp=> (vproc number_1 (action-stack v_acts ...) fiber-queue_1 (unmasked) (in-hole E_1 e_1))
;           ,(term-let ((x_new (variable-not-in (term (in-hole E_1 e_1)) (term x))))
;                      (term (vproc number_1 (action-stack v_acts ...) fiber-queue_1 (masked)
;                                            (forward (preempt (λ (x_new) (in-hole E_1 e_1)))))))
;           "preempt")
          
     (vp=> (vproc number_1 action_1 (queue v_ks ... v_k) mask_1 (in-hole E_1 (deq-vp)))
           (vproc number_1 action_1 (queue v_ks ...) mask_1 (in-hole E_1 v_k))
           "deq-vp")
     
     (vp=> (vproc number_1 action_1 (queue v_ks ...) mask_1 (in-hole E_1 (enq-vp v_k)))
           (vproc number_1 action_1 (queue v_k v_ks ...) mask_1 (in-hole E_1 (unit)))
           "enq-vp")
           
     ;; multiproc transitions
     
     (--> ((vps VP_1 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (let ((x_1 (ref v_1))) e_1)))
                VP_2 ...)
           (store (x_s v_s) ...) provision-map_1)
          ,(term-let ((x_new (variable-not-in (term ((x_s v_s) ...)) (term x_1))))
                     (term ((vps VP_1 ...
                                 (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (subst (x_1 x_new e_1))))
                                 VP_2 ...)
                            (store (x_new v_1) (x_s v_s) ...) provision-map_1)))
          "ref")
     
     (--> ((vps VP_1 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (deref x_1)))
                VP_2 ...)
           (store (x_s1 v_s1) ... (x_1 v_1) (x_s2 v_s2) ...) provision-map_1)
          ((vps VP_1 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 v_1))
                VP_2 ...)
           (store (x_s1 v_s1) ... (x_1 v_1) (x_s2 v_s2) ...) provision-map_1)
          "deref")     
            
     (--> ((vps VP_1 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (cas x_l v_o v_n)))
                VP_2 ...)
           (store (x_s1 v_s1) ... (x_1 v_1) (x_s2 v_s2) ...) provision-map_1)
          ((vps VP_1 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 v_1))
                VP_2 ...)
           (store (x_s1 v_s1) ... (x_1 ,(if (= (term v_1) (term v_o))
                                            (term v_n) 
                                            (term v_1))
                  (x_s2 v_s2) ...))
                  provision-map_1)
          "cas")
     
     (--> ((vps VP_1 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (enq-on-vp number_2 v_k)))
                VP_2 ...
                (vproc number_2 action_2 (queue v_ks2 ...) mask_2 e_2)
                VP_3 ...)
           global-store_1 provision-map_1)
          ((vps VP_1 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (unit)))
                VP_2 ...
                (vproc number_2 action_2 (queue v_k v_ks2 ...) mask_2 e_2)
                VP_3 ...)
           global-store_1 provision-map_1)
          "enq-on-vp-l")
     
     (--> ((vps VP_1 ...
                (vproc number_2 action_2 (queue v_ks2 ...) mask_2 e_2)
                VP_2 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (enq-on-vp number_2 v_k)))
                VP_3 ...)
           global-store_1 provision-map_1)
          ((vps VP_1 ...
                (vproc number_2 action_2 (queue v_k v_ks2 ...) mask_2 e_2)
                VP_2 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (unit)))
                VP_3 ...)
           global-store_1 provision-map_1)
          "enq-on-vp-r")
     
     (--> ((vps VP_1 ...
                (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 (gid)))
                VP_2 ...)                
           global-store_1
           (pmap (number_p1 number_p2 ...) ... ))
          ,(term-let ((number-gid (gid (term (number_p1 ...)))))
                     (term
                      ((vps VP_1 ...
                            (vproc number_1 action_1 fiber-queue_1 mask_1 (in-hole E_1 number-gid))
                            VP_2 ...)                
                       global-store_1
                       (pmap (number_p1 number_p2 ...) ... (number-gid)))))
          "gid")
     
     (--> ((vps VP_1 ...
                (vproc number_1 (action-stack v_a1 ...) fiber-queue_1 mask_1 (in-hole E_1 (provision number_gid)))
                VP_2 ...)                
           global-store_1
           (pmap (number_p1 number_p2 ...) ... (number_gid number_vps ...) (number_p3 number_p4 ...) ... ))
          ,(term-let ((number-vp (provision (term number_1) 
                                            (append (term (VP_1 ...)) (term (VP_2 ...))) 
                                            (term (number_vps ...)))))
                     (term
                      ((vps VP_1 ...
                            (vproc number_1 (action-stack v_a1 ...) fiber-queue_1 mask_1 (in-hole E_1 number-vp))
                            VP_2 ...)                
                       global-store_1
                       (pmap (number_p1 number_p2 ...) ... (number_gid number-vp number_vps ...) 
                             (number_p3 number_p4 ...) ... ))))
          "provision")          
      
     where
     ; sequential transition rule
     ((s==> e1 e2) (--> ((vps VP_1 ... (vproc number_vp (action-stack v_a ...) fiber-queue_1 mask_1 e1)
                                              VP_2 ...) (store (x_1 v_1) ...) (pmap (number_1 number_2 ...) ...))
                        ((vps VP_1 ... (vproc number_vp (action-stack v_a ...) fiber-queue_1 mask_1 e2)
                                             VP_2 ...) (store (x_1 v_1) ...) (pmap (number_1 number_2 ...) ...))))
     ; vproc transition rule
     ((vp=> vp1 vp2) (-->  ((vps VP_1 ... vp1 VP_2 ...) (store (x_1 v_1) ...) (pmap (number_1 number_2 ...) ...))
                           ((vps VP_1 ... vp2 VP_2 ...) (store (x_1 v_1) ...) (pmap (number_1 number_2 ...) ...))))))
    
    ; BUG: ((s==> e1 e2) (vp=> (number_1 (action-stack v_a ...) fiber-queue_1 e1)
    ;                    (number_1 (action-stack v_a ...) fiber-queue_1 e2)))))
     
  (define-metafunction subst
    scheduler-language
    [(x_1 x_1 e_1)
     e_1]
    [(x_1 e_1 (λ (x_1) e_2))
     (λ (x_1) e_2)]
    [(x_1 e_1 (λ (x_2) e_2))
     ,(term-let ([x_new (variable-not-in (term e_1) (term x_2))])
                (term
                 (λ (x_new) (subst (x_1 e_1 (subst (x_2 x_new e_2)))))))]
    [(x_1 e_1 x_1) 
     e_1]
    [(x_1 e_1 x_2) 
     x_2]
    [(x_1 e_1 (e_2 e_3))
     ((subst (x_1 e_1 e_2)) (subst (x_1 e_1 e_3)))]
    [(x_1 e_1 (+ e_2 e_3))
     (+ (subst (x_1 e_1 e_2)) (subst (x_1 e_1 e_3)))]
    [(x_1 e_1 (abort e_2))
     (abort (subst (x_1 e_1 e_2)))]
    [(x_1 e_1 (letcont x_2 x_3 e_2 e_3))
     (letcont x_2 x_3 (subst (x_1 e_1 e_2)) (subst (x_1 e_1 e_3)))]
    [(x_1 e_1 (if0 e_2 e_3 e_4)) 
     (if0 (subst (x_1 e_1 e_2)) 
          (subst (x_1 e_1 e_3))
          (subst (x_1 e_1 e_4)))]
    [(x_1 e_1 number_1)
     number_1]
    [(x_1 e_1 (unit))
     (unit)]
    [(x_1 e_1 (let ((x_1 e_2)) e_3))
     (let ((x_1 e_2)) e_3)]
    [(x_1 e_1 (let ((x_2 e_2)) e_3))
     (let ((x_2 (subst (x_1 e_1 e_2)))) (subst (x_1 e_1 e_3)))]
    [(x_1 e_1 (letrec ((x_1 e_2)) e_3))
     (letrec ((x_1 e_2)) e_3)]
    [(x_1 e_1 (letrec ((x_2 e_2)) e_3))
     (letrec ((x_2 (subst (x_1 e_1 e_2)))) (subst (x_1 e_1 e_3)))]
    [(x_1 e_1 (begin e_s ...))
     (begin (subst (x_1 e_1 e_s)) ...)]
    [(x_1 e_1 (fix e_2))
     (fix (subst (x_1 e_1 e_2)))]
    ;; vproc signals
    [(x_1 e_1 (stop))
     (stop)]
    [(x_1 e_1 (preempt e_k))
     (preempt (subst (x_1 e_1 e_k)))]
    ;; vproc operations
    [(x_1 e_1 (run e_act e_k))
     (run (subst (x_1 e_1 e_act)) (subst (x_1 e_1 e_k)))]
    [(x_1 e_1 (forward e_sig))
     (forward (subst (x_1 e_1 e_sig)))]
    [(x_1 e_1 (handle e_sig (stop-handler e_s) (preempt-handler e_p)))
     (handle (subst (x_1 e_1 e_sig))
             (stop-handler (subst (x_1 e_1 e_s)))
             (preempt-handler (subst (x_1 e_1 e_p))))]
    ;; thread queue operations
    [(x_1 e_1 (deq-vp))
     (deq-vp)]
    [(x_1 e_1 (enq-vp e_2))
     (enq-vp (subst (x_1 e_1 e_2)))]
    [(x_1 e_1 (enq-on-vp e_2 e_3))
     (enq-on-vp (subst (x_1 e_1 e_2)) (subst (x_1 e_1 e_3)))]
    ;; atomic store operations
    [(x_1 e_1 (cas e_2 e_3 e_4))
     (cas (subst (x_1 e_1 e_2)) (subst (x_1 e_1 e_3)) (subst (x_1 e_1 e_4)))]
    [(x_1 e_1 (ref e_2))
     (ref (subst (x_1 e_1 e_2)))]
    [(x_1 e_1 (deref e_2))
     (deref (subst (x_1 e_1 e_2)))]
    ;; provisioning operations
    [(x_1 e_1 (gid))
     (gid)]
    [(x_1 e_1 (provision e_2))
     (provision (subst (x_1 e_1 e_2)))]
    [(x_1 e_1 (release e_2))
     (release (subst (x_1 e_2 e_2)))]    
    [(x_1 e_1 (enq e_vp e_k))
     (enq (subst (x_1 e_1 e_vp)) (subst (x_1 e_1 e_k)))]   
    [(x_1 e_1 (deq e_vp))
     (deq (subst (x_1 e_1 e_vp)))])
  
  (define (run-on-vp n e)
    `(vproc ,n (action-stack) (queue) (masked) ,e))
    
  (define (run es)
    (let* ([run (lambda (e vps)
                 (let ([n (car vps)]
                       [es (cdr vps)])
                   (cons (add1 n) (cons (run-on-vp n e) es))))]
          [vs (foldl run (cons 0 '()) es)]
          [vps (reverse (cdr vs))]
          [mm `((vps ,@vps) (store) (pmap))])
      (traces scheduler-language multiprocessor-machine mm)))
  
  (define (rr es)
      (let* ([run (lambda (e vps)
                 (let ([n (car vps)]
                       [es (cdr vps)])
                   (cons (add1 n) (cons (run-on-vp n e) es))))]
          [vs (foldl run (cons 0 '()) es)]
          [vps (reverse (cdr vs))]
          [mm `((vps ,@vps) (store) (pmap))])
      (apply-reduction-relation* multiprocessor-machine mm)))
  
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
    (run (list (term (+ (+ 500 3) 2)) (term (+ 3 4)))))
 
  (define (t4)
    (run (list (term (+ 4 3)) (term (+ 59 (letcont k x (+ x 1) (+ 2 (k 1))))))))
  
  (define (t5)
    (run (list (term (enq-on-vp 1 43)) (term (let ((g 6)) (provision g))))))
  
  (define (t6)
    (run (list (term (let ((x (gid))) 
                       (let ((p (provision x))) 
                         (enq-on-vp p 99))))
               (term 888))))
  
  (define (fiber f)
    (term (letcont k x (begin (,f (unit)) (forward (stop))) k)))
  
  (define default-action
    (term (letrec ((act (λ (sig)
                        (handle sig
                                (stop-handler (run act (deq-vp)))
                                (preempt-handler (λ (k) (begin
                                                          (enq-vp k)
                                                          (run act (deq-vp)))))))))
            act)))
  
  (define t
    (term (λ (x) (let ((k ,(fiber (term (λ (x) 999)))))
                   (enq-vp k)))))
  
  (define (test-vp f)
    (run (list (term (run ,default-action ,(fiber f))))))
  )
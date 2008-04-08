(module scheduling-framework mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 4 4))
           (planet "gui.ss" ("robby" "redex.plt" 4 4))
           (planet "subst.ss" ("robby" "redex.plt" 4 4))
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "plt-match.ss"))
  
  (define-language lang
    (MP ((VP ...) global-store provision-map))               ; multiprocessor state
    (provision-map (pmap (fls vproc-id ...) ...))            ; provision map
    (fls store-location)                                     ; fiber-local storage
    (global-store (store (store-location v) ...))            ; global store
    (store-location number)
    (VP (vp vproc-id actions thread-queue mask fls e))       ; vproc state
    (vproc-id number)
    (mask number)                                            ; signal mask (0 for unmasked, > 0 for masked)
    (actions (astk v ...))                                   ; action stack
    (fiber-queue (fq v ...))                                 ; fiber queue
    (signal (stop) (preempt e))                              ; signals for fibers
    (v number (λ (x ...) e) (unit) signal)
    (x variable)
    (e 
     x v (e e ...) (if e e e) (begin e e ...) (let ((x e)) e) (letrec ((x e)) e) (fix e) (fun (x x ...) e e)
     (abort e) (letcont x x ... e e)
     (run e e) (forward e)
     (deq-vp) (enq-on-vp e e) 
     (mask-preemption) (unmask-preemption) (host-vp)
     (new-fls) (set-fls) (get-fls)
     (provision e) (release e e)
     (ref e) (deref e) (cas e e e)
     (handle e (stop-handler e) (preempt-handler e)))     
    (E hole (v ... E e ...) (if E e e) (begin E e e ...) (let ((x E)) e)
       (enq E e) (enq v E) (deq E) 
       (run E e) (run v E) (forward E)
       (enq-on-vp E e) (enq-on-vp v E) 
       (provision E) (release E e) (release v E) 
       (ref E) (deref E) (fix E)
       (cas E e e) (cas number E e) (cas number v E) 
       (handle E (stop-handler e) (preempt-handler e)) 
       (handle v (stop-handler E) (preempt-handler e))
       (handle v (stop-handler v) (preempt-handler E))))
  
  (define multiprocessor-machine
    (reduction-relation
     lang
     
     (--> (in-hole E_1 ((λ (x_1 ...) e_1) v_1 ...))
          (in-hole E_1 (multi-subst ((x_1 ...) (v_1 ...) e_1)))
          "βv")
     
     ))
  
  (define-metafunction multi-subst
    lang
    [(() () e_3) e_3]
    [((x_1 x_2 ...) (e_1 e_2 ...) e_3)
     (multi-subst ((x_2 ...) (e_2 ...) (ssubst (x_1 e_1 e_3))))])
  
  (define-metafunction ssubst
    lang
    [(x_1 x_1 e_1)
     e_1]
    [(x_1 e_1 (λ (x_0 ... x_1 x_2 ...) e_2))
     (λ (x_0 ... x_1 x_2 ...) e_2)]
    [(x_1 e_1 (λ (x_2 ...) e_2))
     ,(term-let ([(xs ...) (map 
                            (λ (v) (variable-not-in (term (e_1 x_2 ...)) v)) 
                            (term (x_2 ...)))])
                (term
                 (λ (xs ...) (ssubst (x_1 e_1 (multi-subst ((x_2 ...) (xs ...) e_2)))))))]
    [(x_1 e_1 (fun (x_f x_ps ...) e_2 e_3))
     ,(term-let ((x-f (variable-not-in (term (e_1 x_1)) (term x_f)))
                ((x-ps ...) (map (λ (v) (variable-not-in (term (e_1 x_1 x_f)) v)) (term (x_ps ...)))))
                          (term
                           (fun (x-f x-ps ...) (multi-subst ((x_f x_ps ... x_1) (x-f x-ps ... e_1) e_2))
                                    (multi-subst ((x_f x_1) (x-f e_1) e_3)))))]
    [(x_1 e_1 x_1) 
     e_1]
    [(x_1 e_1 x_2) 
     x_2]
    [(x_1 e_1 (e_2 e_3 ...))
     ((ssubst (x_1 e_1 e_2)) (ssubst (x_1 e_1 e_3)) ...)]
    [(x_1 e_1 (abort e_2))
     (abort (ssubst (x_1 e_1 e_2)))]
    [(x_1 e_1 (letcont x_2 x_3 ... e_2 e_3))
     ,(term-let ((x-new2 (variable-not-in (term (e_1 x_1)) (term x_2)))
                ((x-new3 ...) (map (λ (v) (variable-not-in (term (e_1 x_1 x_2)) v)) (term (x_3 ...)))))
                          (term
                           (letcont x-new2 x-new3 ... (multi-subst ((x_2 x_3 ... x_1) (x-new2 x-new3 ... e_1) e_2))
                                    (multi-subst ((x_2 x_1) (x-new2 e_1) e_3)))))]                                    
    [(x_1 e_1 (if e_2 e_3 e_4)) 
     (if (ssubst (x_1 e_1 e_2)) 
         (ssubst (x_1 e_1 e_3))
         (ssubst (x_1 e_1 e_4)))]
    [(x_1 e_1 number_1)
     number_1]
    [(x_1 e_1 (unit))
     (unit)]
    [(x_1 e_1 (let ((x_2 e_2)) e_3))
     ,(term-let ((x-new (variable-not-in (term (e_1 x_1)) (term x_2))))
                (term
                 (let ((x-new (ssubst (x_1 e_1 e_2)))) 
                   (multi-subst ((x_2 x_1) (x-new e_1) e_3)))))]
    [(x_1 e_1 (letrec ((x_2 e_2)) e_3))
     ,(term-let ((x-new (variable-not-in (term (e_1 x_1)) (term x_2))))
                (term 
                 (letrec ((x-new (multi-subst ((x_2 x_1) (x-new x_1) e_2))))
                   (multi-subst ((x_2 x_1) (x-new e_1) e_3)))))]
    [(x_1 e_1 (begin e_s ...))
     (begin (ssubst (x_1 e_1 e_s)) ...)]
    [(x_1 e_1 (fix e_2))
     (fix (ssubst (x_1 e_1 e_2)))]
    ;; vproc signals
    [(x_1 e_1 (stop))
     (stop)]
    [(x_1 e_1 (preempt e_k))
     (preempt (ssubst (x_1 e_1 e_k)))]
    ;; vproc operations
    [(x_1 e_1 (run e_act e_k))
     (run (ssubst (x_1 e_1 e_act)) (ssubst (x_1 e_1 e_k)))]
    [(x_1 e_1 (forward e_sig))
     (forward (ssubst (x_1 e_1 e_sig)))]
    [(x_1 e_1 (handle e_sig (stop-handler e_s) (preempt-handler e_p)))
     (handle (ssubst (x_1 e_1 e_sig))
             (stop-handler (ssubst (x_1 e_1 e_s)))
             (preempt-handler (ssubst (x_1 e_1 e_p))))]
    ;; thread queue operations
    [(x_1 e_1 (deq-vp))
     (deq-vp)]
    [(x_1 e_1 (enq-vp e_2))
     (enq-vp (ssubst (x_1 e_1 e_2)))]
    [(x_1 e_1 (enq-on-vp e_2 e_3))
     (enq-on-vp (ssubst (x_1 e_1 e_2)) (ssubst (x_1 e_1 e_3)))]
    [(x_1 e_1 (queue v_ks ...))
     (queue (ssubst (x_1 e_1 v_ks)) ...)]
    [(x_1 e_1 (mask-preemption))
     (mask-preemption)]
    ;; atomic store operations
    [(x_1 e_1 (cas e_2 e_3 e_4))
     (cas (ssubst (x_1 e_1 e_2)) (ssubst (x_1 e_1 e_3)) (ssubst (x_1 e_1 e_4)))]
    [(x_1 e_1 (ref e_2))
     (ref (ssubst (x_1 e_1 e_2)))]
    [(x_1 e_1 (deref e_2))
     (deref (ssubst (x_1 e_1 e_2)))]
    ;; provisioning operations
    [(x_1 e_1 (provision e_2))
     (provision (ssubst (x_1 e_1 e_2)))]
    [(x_1 e_1 (release e_2 e_3))
     (release (ssubst (x_1 e_1 e_2)) (ssubst (x_1 e_1 e_3)))]        
    [(x_1 e_1 (host-vp))
     (host-vp)]
    [(x_1 e_1 (enq e_vp e_k))
     (enq (ssubst (x_1 e_1 e_vp)) (ssubst (x_1 e_1 e_k)))]   
    [(x_1 e_1 (deq e_vp))
     (deq (ssubst (x_1 e_1 e_vp)))]
    )
    
  (define (trace e)
    (traces lang multiprocessor-machine e))
  
  (define e1
    (term ((λ (x) 1) (λ (y) 1))))

  )
  
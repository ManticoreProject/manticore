(module scheduling-framework mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 4))
           (planet "gui.ss" ("robby" "redex.plt" 4))
           (planet "subst.ss" ("robby" "redex.plt" 4))
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "plt-match.ss")
           )
  
  (provide lang multiprocessor-machine make-multiprocessor mp-stepper)
  
  (define-language lang
    (MP ((VP ...) global-store provision-map))               ; multiprocessor state
    (VP (vp vproc-id actions fiber-queue mask fls e))        ; vproc state
    (provision-map (pmap (fls vproc-id ...) ...))            ; provision map
    (fls store-location)                                     ; fiber-local storage
    (global-store (store (store-location v) ...))            ; global store
    (store-location number)
    (vproc-id number)
    (mask number)                                            ; signal mask (0 for unmasked, > 0 for masked)
    (actions (astk v ...))                                   ; action stack
    (fiber-queue (fq v ...))                                 ; fiber queue
    (signal (stop) (preempt e))                              ; signals for fibers
    (v number (λ (x ...) e) (unit) signal (ffi-val any) string)
    (x (variable-except λ unit  stop preempt if begin let letrec fix fun
                        ffi-call ffi-val ffi-peek
                        abort letcont 
                        run forward
                        deq-vp enq-on-vp 
                        mask-preemption unmask-preemption host-vp
                        new-fls set-fls get-fls get-from-fls set-in-fls
                        provision release
                        ref deref cas
                        handle stop-handler preempt-handler
                        vp pmap store astk fq
                        ))                                  ; language variables must not include primops
    (e 
     x v (e e ...) (begin e e ...) (if e e e) (let ((x e)) e) (letrec ((x e)) e) (fix e) (fun (x x ...) e e)
     (ffi-call x e ...) (ffi-peek e)
     (abort e) (letcont x x ... e e)
     (run e e) (forward e)
     (deq-vp) (enq-on-vp e e) 
     (mask-preemption) (unmask-preemption) (host-vp)
     (new-fls) (set-fls e) (get-fls) (get-from-fls e e) (set-in-fls e e e)
     (provision e) (release e e)
     (ref e) (deref e) (cas e e e)
     (handle e (stop-handler e) (preempt-handler e))
     )     
    (E hole (v ... E e ...) (if E e e) (begin E e e ...) (let ((x E)) e)
       (ffi-call x v ... E e ...) (ffi-peek E)
       (set-fls E) (get-from-fls E e) (get-from-fls v E) (set-in-fls E e e) (set-in-fls v E e) (set-in-fls v v E)
       (run E e) (run v E) (forward E)
       (enq-on-vp E e) (enq-on-vp v E) 
       (provision E) (release E e) (release v E) 
       (ref E) (deref E) (fix E)
       (cas E e e) (cas number E e) (cas number v E) 
       (handle E (stop-handler e) (preempt-handler e)) 
       (handle v (stop-handler E) (preempt-handler e))
       (handle v (stop-handler v) (preempt-handler E))))
  
  ; peek : e -> e
  ; extract the Scheme value from an ffi-val object
  (define-metafunction peek
    lang
    [(ffi-val any_1) any_1]
    [e_1 e_1])
  
  ;; get-vp-id : VP -> vproc-id
  ;; extract the vproc id from a vproc machine
  (define (get-vp-id vp)
    (match vp
      (`(vp ,id ,s ,q ,m ,fls ,e) id)))
  
  ;; get-fls-dict : global-store * fls -> fls-lookup-dict
  ;; find the fls entry in the global store and return the dictionary
  (define (get-fls-dict store fls)
    (let ((fls-dict (assoc fls store)))
      (match fls-dict
        (#f (error "cannot find fls"))
        (`(,loc (ffi-val ,dict)) dict))))
  
  ;; remove-dict : e -> listof(e*d) -> listof(e*d)
  ;; remove any elements from the dictionary d with the key y
  (define (remove-dict y d)
    (filter (lambda (x) (not (equal? (car x) y))) d))
  
  ;;; Multiprocessor machine
  ;;;   This multiprocessor machine (-->) consists of component machines
  ;;;     * sequential machine     (s==>)
  ;;;     * vproc machine          (vp=>)
  (define multiprocessor-machine
    (reduction-relation
     lang
     ;;; sequential machine
     (s==> (in-hole E_1 ((λ (x_1 ...) e_1) v_1 ...))
           (in-hole E_1 (multi-subst ((x_1 ...) (v_1 ...) e_1)))
           "βv")
     (s==> (in-hole E_1 (ffi-call x_1 v_2 ...))
           (in-hole E_1 (ffi-val ,(eval (term (x_1 '(peek v_2) ...)))))
           "ffi-call")
     (s==> (in-hole E_1 (ffi-peek (ffi-val v_1)))
           (in-hole E_1 v_1)
           "ffi-peek")
     (s==> (in-hole E_1 (fix (λ (x_1) e_1)))
           (in-hole E_1 (ssubst (x_1 (fix (λ (x_1) e_1)) e_1)))
           "fix")
     (s==> (in-hole E_1 (abort e_1))
           e_1
           "abort")
     (s==> (in-hole E_1 (letcont x_1 x_2 ... e_1 e_2))
           (in-hole E_1 (letrec ((x_1 (λ (x_2 ...) (abort (in-hole E_1 e_1))))) e_2))
           "letcont")
     (s==> (in-hole E_1 (let ((x_1 v_1)) e_2))
           (in-hole E_1 (ssubst (x_1 v_1 e_2)))
           "let")
     (s==> (in-hole E_1 (letrec ((x_1 e_1)) e_2))
           (in-hole E_1 (let ((x_1 (fix (λ (x_1) e_1)))) e_2))
           "letrec")
     (s==> (in-hole E_1 (fun (x_f x_1 ...) e_1 e_2))
           (in-hole E_1 (letrec ((x_f (λ (x_1 ...) e_1))) e_2))
           "fun")
     (s==> (in-hole E_1 (if number_1 e_1 e_2))
           (in-hole E_1 e_1)
           (side-condition (not (= 0 (term number_1))))
           "if-t")     
     (s==> (in-hole E_1 (if 0 e_1 e_2))
           (in-hole E_1 e_2)
           "if-f")
     (s==> (in-hole E_1 (begin v_1 e_1 e_2 ...))
           (in-hole E_1 (begin e_1 e_2 ...))
           "begin-many")
     (s==> (in-hole E_1 (begin e_1))
           (in-hole E_1 e_1)
           "begin-one")
     (s==> (in-hole E_1 (handle (stop)
                                (stop-handler v_s)
                                (preempt-handler v_k)))
           (in-hole E_1 (v_s))
           "handle-stop")
     (s==> (in-hole E_1 (handle (preempt v_k)
                                (stop-handler v_s)
                                (preempt-handler v_ph)))
           (in-hole E_1 (v_ph v_k))
           "handle-preempt")
     ;;; vproc machine
     (vp=> (vp number_1 (astk v_act1 ...) fiber-queue_1 mask_1 fls_1 (in-hole E_1 (run v_act2 (λ () e_1))))
           (vp number_1 (astk v_act2 v_act1 ...) fiber-queue_1 0 fls_1 e_1)
      ;     (side-condition (not (= (term mask_1) 0)))
           "run")
     (vp=> (vp number_1 (astk v_act v_acts ...) fiber-queue_1 mask_1 fls_1 (in-hole E_1 (forward v_sig)))
           (vp number_1 (astk v_acts ...) fiber-queue_1 ,(add1 (term mask_1)) fls_1 (v_act v_sig))
           "forward")
     (vp=> (vp number_1 (astk v_act v_acts ...) fiber-queue_1 0 fls_1 e_1)
           (vp number_1 (astk v_acts ...) fiber-queue_1 1 fls_1 (v_act (preempt (λ () e_1))))
           "preempt")
     (vp=> (vp number_1 actions_1 (fq v_ks ... v_k) mask_1 fls_1 (in-hole E_1 (deq-vp)))
           (vp number_1 actions_1 (fq v_ks ...) mask_1 fls_1 (in-hole E_1 v_k))
           "deq-vp")
     (vp=> (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (mask-preemption)))
           (vp number_1 actions_1 fiber-queue_1 ,(add1 (term mask_1)) fls_1 (in-hole E_1 (unit)))
           "mask-preemption")
     (vp=> (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (unmask-preemption)))
           (vp number_1 actions_1 fiber-queue_1 ,(sub1 (term mask_1)) fls_1 (in-hole E_1 (unit)))
           "unmask-preemption")
     (vp=> (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (host-vp)))
           (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 number_1))
           "host-vp")
     (vp=> (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (get-fls)))
           (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 fls_1))
           "get-fls")
     (vp=> (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (set-fls v_1)))
           (vp number_1 actions_1 fiber-queue_1 mask_1 v_1 (in-hole E_1 (unit)))
           "set-fls")
     ;;; multiprocessor machine
     ; get the fls entry for string_tag in fls_2.  if this entry does not yet exist, create it here.
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (get-from-fls fls_2 string_tag)))
            VP_2 ...)
           (store (number_l v_l) ... (fls_2 (ffi-val any_fls)) (number_r v_r) ...) 
           provision-map_1)
          ,(term-let ((elt (assoc (term string_tag) (term any_fls))))
                     (if (term elt)
                         (term
                          ((VP_1 ...
                            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 ,(cdr (term elt))))
                            VP_2 ...)
                           (store (number_l v_l) ... (fls_2 (ffi-val any_fls)) (number_r v_r) ...) 
                           provision-map_1))
                         (term-let ((loc (add1 (caar (reverse (term ((fls_2 (ffi-val any_fls)) (number_r v_r) ...)))))))
                                   (term
                                    ((VP_1 ...
                                      (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 loc))
                                      VP_2 ...)
                                     (store (number_l v_l) ... (fls_2 (ffi-val ,(cons (cons (term string_tag) (term loc)) (term any_fls)))) (number_r v_r) ... (loc (unit)))
                                     provision-map_1)))))
          "get-from-fls")
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (set-in-fls fls_2 string_tag v_1)))
            VP_2 ...)
           (store (number_l v_l) ... (fls_2 (ffi-val any_fls)) (number_r v_r) ...) 
           provision-map_1)
          ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (unit)))
            VP_2 ...)
           (store (number_l v_l) ... (fls_2 (ffi-val ,(cons (cons (term string_tag) (term v_1))
                                                                  (remove-dict (term string_tag) (term any_fls)))))
                  (number_r v_r) ...)
           provision-map_1)
          "set-in-fls")
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (let ((x_1 (ref v_1))) e_1)))
            VP_2 ...)
           (store (number_l v_s) ... (number_r v_r)) provision-map_1)
          ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (let ((x_1 ,(add1 (term number_r)))) e_1)))
            VP_2 ...)
           (store (number_l v_s) ... (number_r v_r) (,(add1 (term number_r)) v_1)) provision-map_1)
          "ref")          
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (deref number_l)))
            VP_2 ...)
           (store (number_s1 v_s1) ... (number_l v_1) (number_s2 v_s2) ...) provision-map_1)
          ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 v_1))
            VP_2 ...)
           (store (number_s1 v_s1) ... (number_l v_1) (number_s2 v_s2) ...) provision-map_1)
          "deref")                 
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (cas number_l v_o v_n)))
            VP_2 ...)
           (store (number_s1 v_s1) ... (number_l v_1) (number_s2 v_s2) ...) provision-map_1)
          ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 v_1))
            VP_2 ...)
           (store (number_s1 v_s1) ... (number_l ,(if (equal? (term v_1) (term v_o))
                                                      (term v_n) 
                                                      (term v_1)))
                  (number_s2 v_s2) ...)
           provision-map_1)
          "cas")
     (--> ((VP_1 ...
            (vp number_1 actions_1 (fq v_ks ...) mask_1 fls_1 (in-hole E_1 (enq-on-vp number_1 v_k)))
            VP_3 ...)
           global-store_1 provision-map_1)
          ((VP_1 ...
            (vp number_1 actions_1 (fq v_k v_ks ...) mask_1 fls_1 (in-hole E_1 (unit)))
            VP_3 ...)
           global-store_1 provision-map_1)
          "enq-vp-self")     
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (enq-on-vp number_2 v_k)))
            VP_2 ...
            (vp number_2 actions_2 (fq v_ks2 ...) mask_2 fls_2 e_2)
            VP_3 ...)
           global-store_1 provision-map_1)
          ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (unit)))
            VP_2 ...
            (vp number_2 actions_2 (fq v_k v_ks2 ...) mask_2 fls_2 e_2)
            VP_3 ...)
           global-store_1 provision-map_1)
          "enq-on-vp-l")     
     (--> ((VP_1 ...
            (vp number_2 actions_2 (fq v_ks2 ...) mask_2 fls_2 e_2)
            VP_2 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (enq-on-vp number_2 v_k)))
            VP_3 ...)
           global-store_1 provision-map_1)
          ((VP_1 ...
            (vp number_2 actions_2 (fq v_k v_ks2 ...) mask_2 fls_2 e_2)
            VP_2 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (unit)))
            VP_3 ...)
           global-store_1 provision-map_1)
          "enq-on-vp-r")
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (provision number_fid)))
            VP_2 ...)                
           global-store_1
           (pmap (number_p1 number_p2 ...) ... 
                 (number_fid number_vp number_vps ...) 
                 (number_p3 number_p4 ...) ... ))
          ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 number_vp))
            VP_2 ...)                
           global-store_1
           (pmap (number_p1 number_p2 ...) ... 
                 (number_fid number_vps ...) 
                 (number_p3 number_p4 ...) ... ))
          "provision")     
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (release number_fid number_vp)))
            VP_2 ...)                
           global-store_1
           (pmap (number_p1 number_p2 ...) ... (number_fid number_vps ...) (number_p3 number_p4 ...) ... ))
          ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (unit)))
            VP_2 ...)                
           global-store_1
           (pmap (number_p1 number_p2 ...) ... 
                 (number_fid number_vp number_vps ...) 
                 (number_p3 number_p4 ...) ... ))
          (side-condition (not (memq (term number_vp) (term (number_vps ...)))))
          "release")     
     ; simultaneously allocate fls in the store and add a placeholder to the provision map
     (--> ((VP_1 ...
            (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 (new-fls)))
            VP_2 ...)                
           (store (number_ls v_s) ... (number_r v_r))
           (pmap (number_p1 number_p2 ...) ... ))
          ,(term-let ((fls-loc (add1 (term number_r)))
                      ((all-vp-ids ...) (cons (term number_1) (map get-vp-id (term (VP_1 ... VP_2 ...))))))
                     (term
                      ((VP_1 ...
                        (vp number_1 actions_1 fiber-queue_1 mask_1 fls_1 (in-hole E_1 fls-loc))
                        VP_2 ...)                
                       (store (number_ls v_s) ... (number_r v_r) (fls-loc (ffi-val ())))
                       (pmap (number_p1 number_p2 ...) ... (fls-loc all-vp-ids ...)))))
          "new-fls")     
     
     where
     ; sequential evaluation (hosted by the vproc machine)
     ((s==> e1 e2) (vp=> (vp vproc-id_1 actions_1 fiber-queue_1 mask_1 fls_1 e1)
                         (vp vproc-id_1 actions_1 fiber-queue_1 mask_1 fls_1 e2)))

     ; vproc evaluation (hosted by the multiprocessor machine)
     ((vp=> vp1 vp2) (--> ((VP_1 ... vp1 VP_2 ...) global-store_1 provision-map_1)
                          ((VP_1 ... vp2 VP_2 ...) global-store_1 provision-map_1)))
     ))
  
  ; multi-subst : listof(x) * listof(e) * e -> e
  ; capture-avoiding substitution lifted to lists
  (define-metafunction multi-subst
    lang
    [(() () e_3) e_3]
    [((x_1 x_2 ...) (e_1 e_2 ...) e_3)
     (multi-subst ((x_2 ...) (e_2 ...) (ssubst (x_1 e_1 e_3))))])
  
  ; subst : x * e -> e
  ; capture-avoiding substitution
  (define-metafunction ssubst
    lang
    [(x_1 x_1 e_1)
     e_1]
    [(x_1 e_1 string_1)
     string_1]
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
    ;; fls
    [(x_1 e_1 (new-fls))
     (new-fls)]
    [(x_1 e_1 (get-fls))
     (get-fls)]
    [(x_1 e_1 (set-fls e_2))
     (set-fls (ssubst (x_1 e_1 e_2)))]
    [(x_1 e_1 (get-from-fls e_2 e_3))
     (get-from-fls (ssubst (x_1 e_1 e_2)) (ssubst (x_1 e_1 e_3)))]
    [(x_1 e_1 (set-in-fls e_2 e_3 e_4))
     (set-in-fls (ssubst (x_1 e_1 e_2)) (ssubst (x_1 e_1 e_3)) (ssubst (x_1 e_1 e_4)))]
    ;; ffi
    [(x_1 e_1 (ffi-call e_s ...))
     (ffi-call (ssubst (x_1 e_1 e_s)) ...)]
    [(x_1 e_1 (ffi-peek e_2))
     (ffi-peek (ssubst (x_1 e_1 e_2)))]
    [(x_1 e_1 (ffi-val any_1))
     (ffi-val any_1)]
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
    )
  
  ; make-vproc : int * e -> VP
  ; given a vproc id and an initial expression, construct a vproc machine state
  (define (make-vproc n e)
    (term (vp ,n (astk) (fq) 1 1 ,e)))
  
  ; make-multiprocessor : listof(e) -> MP
  ; given a list of expressions, construct a multipocessor machine state. element e_i in the list
  ; seeds VP_i in the multiprocessor
  (define (make-multiprocessor seed-es)
    (let* (; add a vproc machine state to the machine
           [f (lambda (e vps)
                (let ([n (car vps)]
                      [es (cdr vps)])
                  (cons (add1 n) (cons (make-vproc n e) es))))]
           [vps (reverse (cdr (foldl f (cons 1 '()) seed-es)))])
      ; store locations 0 and 1 must be seeded to start the machine.
      ; location 1 is empty FLS.
      `((,@vps) (store (0 0) (1 (ffi-val ()))) (pmap))))
  
  (define (mp-stepper es)
    (stepper lang multiprocessor-machine es))
  
  (define e1
    (term ((λ (x y) (x y)) (λ (y) 2) 123)))
  
  (define e2
    (term (ffi-call cons 1 2)))
  
  (define e3
    (term ((λ (ls) (ffi-peek (ffi-call car ls))) ,e2)))
  
  (define e-fls
    (term (begin
            (mask-preemption)
            (set-fls (new-fls))
            (provision (get-fls))
            (unmask-preemption))))
  
  (define gfls
    (term (let ((fls (new-fls)))
            (let ((x1 (get-from-fls fls "x")))
                (let ((p (set-in-fls fls "x" 2343)))
                  (get-from-fls fls "x")))
            )))
  
  
  (define test11 (term
    ((
     (vp
      1
      (astk)
      (fq)
      0
      1
      ((λ ()  (cas 0 0 0)))
      )

  )
 (store)
 (pmap))))
  
  )

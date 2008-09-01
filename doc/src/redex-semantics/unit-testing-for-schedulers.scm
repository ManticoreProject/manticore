(module unit-testing-for-schedulers mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 4 4))
           (planet "gui.ss" ("robby" "redex.plt" 4 4))
           (planet "pict.ss" ("robby" "redex.plt" 4 4))
           (planet "test.ss" ("schematics" "schemeunit.plt" 3))
           (lib "plt-match.ss")
           (lib "list.ss")
           (lib "pretty.ss")
           "scheduling-framework.scm"
           "scheduling-utils.scm"
           (planet "random.ss" ("schematics" "random.plt" 1 0)))
  
  (provide check-random-reduction)
  
  (define (pretty-display-term e port w)
    (parameterize ([pretty-print-columns w])
      (pretty-print e port)))
  
  (define (random-path-stepper es)
    (let* ([res (random-reduction-path 1000 multiprocessor-machine (init-top-level-schedulers round-robin es))]
           [reds (car res)]
           [path (cadr res)])
      (stepper/seed lang multiprocessor-machine path 
                    (lambda (e port width text%) (pretty-display-term (remove-fix e) port width))
                    )))

  (define (subset? a b)
    (let ([ht (make-hash-table 'equal)])
      (for-each (λ (x) (hash-table-put! ht x #t)) a)
      (andmap (λ (x) (hash-table-get ht x #f)) b)))
  

  (define (get-store t)
    (match t
      (`(,vprocs ,global-store ,provision-map) global-store)))
  
  (define (hide-stack t)
    (let ((hide (lambda (t)
                  (match t
                    (`(vp ,id ,s ,q ,m ,fls ,e) `(vp ,id (astk) ,q ,m ,fls ,e))))))
      (match t
        (`((,vprocs ...) ,global-store ,provision-map) `((,@(map hide vprocs)) ,global-store ,provision-map)))))
                                                  
  ;; remove the body of the "fix" operator from terms to
  ;; help readability
  (define (remove-fix e)
    (match e
      [(? symbol? s) s]
      [(? number? n) n]
      (`(fix ,e) `(fix))
      (es (if (list? es)
              (map remove-fix es)
              es))))
  
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
  
  ; random-prune-rule: listof(transition) -> string -> int -> listof(state)
  ; given a list of machine transitions, a rule, and an integer p, remove transitions
  ; to the rule with probability 1/p.
  (define (random-prune-rule transitions rule prob)
    (let ([f
           (lambda (r)
             (or (not (equal? (car r) rule))
                 (= (random-integer prob) 0)))])
      (filter f transitions)))
  
  ; 1/5 chance of keeping preempt transitions
  (define (prune-preemption branches)
    (random-prune-rule branches "preempt" 5))
  
  (define (rewind e es)
    (letrec ([r (lambda (rs)
                  (cond 
                    [(null? rs) es]
                    [(equal? e (car rs)) (cdr rs)]
                    [else (r (cdr rs))]))])
      (r es)))

  ; Follow a random path through the reduction graph to a terminal state. This function
  ; naturally avoids infinite reduction sequences, as we expect the random path to eventually
  ; lead the computation out of a cycle.
  (define (random-reduction-path max-reductions reductions exp)
    (let ([answer-tbl (make-hash-table 'equal)]
          [visited (make-hash-table 'equal)])
      (letrec ([loop
                (lambda (parents num-visited)
                  (lambda (exp)
                    (let* ([nexts (apply-reduction-relation reductions exp)]
                           [nexts-with-names (apply-reduction-relation/tag-with-names reductions exp)]
                           [nexts-with-names (prune-preemption nexts-with-names)]
                           [nexts (map cadr nexts-with-names)]
                           [uniqs (mk-uniq nexts)])
                      (unless (= (length uniqs) (length nexts))
                        (error 'random-reduction-path
                               "term ~s produced non unique results:~a ~s ~s"
                               exp
                               (apply
                                string-append
                                (map (λ (x) (format "\n~s" x)) nexts-with-names))
                               (length uniqs) (length nexts)))
                      (unless (< num-visited max-reductions)
                        (error 'random-reduction-path
                               "term ~s produced more than ~s reductions"
                               exp
                               max-reductions))
                      (cond 
                        ; we've reached a stop state, so record it as an answer
                        [(null? uniqs) (hash-table-put! answer-tbl exp parents)]   
                        ; visit one of the next states
                        [else (let ([next-parents (cons exp (if (hash-table-get visited exp #f)
                                                                  (rewind exp parents)
                                                                  parents))])
                                ;(print (apply string-append (map (λ (x) (format "~s " (car x))) nexts-with-names)))
                                (hash-table-put! visited exp #t)  ; record visiting exp
                                (for-each (loop next-parents (add1 num-visited)) (pick-random-elt uniqs)))]))))])
        ((loop '() 0) exp)
        (let ([answers (hash-table-map answer-tbl (lambda (x y) x))]
              [paths (hash-table-map answer-tbl (lambda (x y) (reverse y)))])
          (cons answers paths)))))
  
  ;; mk-uniq : (listof X) -> (listof X)
  ;; returns the uniq elements (according to equal?) in terms.
  (define (mk-uniq terms)
    (let ([ht (make-hash-table 'equal)])
      (for-each (λ (x) (hash-table-put! ht x #t)) terms)
      (hash-table-map ht (λ (k v) k))))
  
  ;; unit testing
  (define-check (check-random-reduction check-name get-result max-reducts rel from tos)
    (let* ([res (random-reduction-path max-reducts rel from)]
           [reds (car res)]
           [path (cadr res)]
           [stepper (lambda () 
                      (stepper/seed lang 
                                    multiprocessor-machine 
                                    path 
                                    (lambda (e port width text%) 
                                      (let ([e (remove-fix e)])
                                        (pretty-display-term e port width)))
                                             ))]
           [anss (map get-result reds)])
      (cond [(null? anss)
             (begin
               (stepper)
               (with-check-info 
                (('empty-result check-name))
                (fail-check)))]
            [else
             (unless (subset? tos anss)
               (begin
                 (stepper)
                 (with-check-info
                  (('incorrect-results check-name))
                  (fail-check))))])))
  )
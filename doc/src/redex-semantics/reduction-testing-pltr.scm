(module reduction-testing-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 26))
           (planet "gui.ss" ("robby" "redex.plt" 3 26))
           (planet "pict.ss" ("robby" "redex.plt" 3 26))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require (lib "plt-match.ss")
           (lib "pretty.ss"))
  (require "schedulers-pltr.scm")
  (require "scheduler-utils-pltr.scm")
  (require (planet "random.ss" ("schematics" "random.plt" 1 0)))

  (provide random-reduction-path check-random-reduction random-path-stepper mk-test-sched remove-fix display-store pretty-display-term hide-stack)
  
;  ,(variable-not-in (term e) (term x-unit))
  (define (test-sched act es)
    (map (lambda (e) 
           (term (run ,act ,(fiber (term (λ () ,e)))))) 
         es))
  
  (define (mk-test-sched sched ts)
    (mk-multiprocessor (test-sched sched ts)))
  
  (define (pretty-display-term e port w)
    (parameterize ([pretty-print-columns w])
      (pretty-print e port)))
  
  (define (random-path-stepper es)
    (let* ([res (random-reduction-path 1000 multiprocessor-machine (mk-test-sched default-action es))]
           [reds (car res)]
           [path (cadr res)])
      (stepper/seed scheduler-language multiprocessor-machine path 
                    (lambda (e port width text%) (pretty-display-term (remove-fix e) port width))
                    )))

  (define (subset? a b)
    (let ([ht (make-hash-table 'equal)])
      (for-each (λ (x) (hash-table-put! ht x #t)) a)
      (andmap (λ (x) (hash-table-get ht x #f)) b)))
  

  (define (display-store t)
    (match t
      (`(,vprocs ,global-store ,provision-map) global-store)))
  
  (define (hide-stack t)
    (let ((hide (lambda (t)
                  (match t
                    (`(vproc ,id ,s ,q ,m ,e) `(vproc ,id (action-stack) ,q ,m ,e))))))
      (match t
        (`((vps ,vprocs ...) ,global-store ,provision-map) `((vps ,@(map hide vprocs)) ,global-store ,provision-map)))))
                                                  
  ;; remove the body of the "fix" operator from terms to
  ;; help readability
  (define (remove-fix e)
    (match e
      [(? symbol? s) s]
      [(? number? n) n]
      (`(fix ,e) `(fix))
      (es (map remove-fix es))))
  
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
  
  (define (rewind e es)
    (letrec ([r (lambda (rs)
                  (cond 
                    [(null? rs) es]
                    [(equal? e (car rs)) (cdr rs)]
                    [else (r (cdr rs))]))])
      (r es)))

  ; Follow a random path through the reduction tree to a terminal state, dropping
  ; circular reduction sequences.  I avoid such circularity by tracking all visited
  ; states, and attempting a new path when one shows up.
  ;; NOTE: I count on random path selection to break circular reduction sequences
  (define (random-reduction-path max-reductions reductions exp)
    (let ([answer-tbl (make-hash-table 'equal)]
          [visited (make-hash-table 'equal)])
      (letrec ([loop
                (lambda (parents num-visited)
                  (lambda (exp)
                    (let* ([nexts (apply-reduction-relation reductions exp)]
                           [nexts-with-names (apply-reduction-relation/tag-with-names reductions exp)]
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
  (define-check (check-random-reduction get-result max-reducts rel from tos)
    (let* ([res (random-reduction-path max-reducts rel from)]
           [reds (car res)]
           [path (cadr res)]
           [stepper (lambda () (stepper/seed scheduler-language 
                                             multiprocessor-machine 
                                             path 
                                             (lambda (e port width text%) (pretty-display-term (remove-fix e) port width))
                                             ))]
           [anss (map get-result reds)])
      (cond [(null? anss)
             (begin
               (stepper)
               (with-check-info 
                (('empty-results reds))
                (fail-check)))]
            [else
             (unless (subset? tos anss)
               (begin
                 (stepper)
                 (with-check-info
                  (('incorrect-results path))
                  (fail-check))))])))
  
  )
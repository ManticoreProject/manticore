(module scheduler-unit-tests mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 4 4))
           (planet "gui.ss" ("robby" "redex.plt" 4 4))
           (planet "pict.ss" ("robby" "redex.plt" 4 4))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "plt-match.ss")
           (lib "pretty.ss")
           "scheduling-framework.scm"
           "scheduling-utils.scm"
           "unit-testing-for-schedulers.scm"
           (planet "random.ss" ("schematics" "random.plt" 1 0)))
  
  (define t1
    (list (set-bang (term 0) ((lift '+) (term 1) (term 2)))))
  
  (define (get-answer m)
    (match m
        (`(,vps (store (0 ,ans) ,global-store ...) ,provision-map) ans)))
  
  (define (do-checks n)
    (let* ([max-reductions 2000]
           [check (lambda (es tos)
                    (check-random-reduction get-answer 
                                            max-reductions 
                                            multiprocessor-machine
                                            (init-top-level-schedulers round-robin es)
                                            tos))])
      (letrec ([check-loop 
                (lambda (i)
                  (unless (< i 0)
                    (begin 
                      ; perform the checks
                      (check t1 '(4))
                      (check-loop (sub1 i)))))])
        (check-loop n))))
  
  )


(module workcrew-scheduler-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 12)))
  (require "schedulers-pltr.scm")
  (require "scheduler-utils-pltr.scm")
  
  (provide workcrew)
  
  (define workcrew
    (term
     (λ (n-jobs)
       (λ (job-fn)
         (letcont k x x
                  (let ((n 2))
                    (let ((vp (provision (gid))))
                      (let ((n-started (ref 1)))
                        (let ((n-done (ref 0)))
                          (letrec ((wc-switch 
                                    (λ (sign)
                                      (let ((next-job (fai n-started)))
                                        (handle sign
                                                (stop-handler 
                                                 (λ (x)
                                                   (if0 (<i next-job n-jobs)
                                                        ,(schedule-fiber (term wc-switch) 
                                                                         (term (λ (x) (job-fn next-job))))
                                                        (if0 (=i (fai n-done) n)
                                                             (k (unit))
                                                             (forward (stop))))))
                                                (preempt-handler (λ (k)
                                                                   (begin ,atomic-yield
                                                                          (run wc-switch k)))))))))
                            (begin
                              ,(dispatch-on (term vp) (term wc-switch))
                              ,(schedule-fiber (term wc-switch)
                                               (term (λ (x) (job-fn 0)))))))))))))))
  )
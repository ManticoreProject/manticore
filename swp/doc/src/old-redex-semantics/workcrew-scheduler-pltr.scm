

(module workcrew-scheduler-pltr mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 3 26)))
  (require "schedulers-pltr.scm")
  (require "scheduler-utils-pltr.scm")
  
  (provide workcrew)
  
  (define n-procs 2)
  
  (define workcrew2
    (term (λ (n-jobs job-fn)
            (letcont k x x
                     (let ((p ,n-procs))
                       (k 1024))))))
    
  (define workcrew
    (term
     (λ (n-jobs job-fn)
       (letcont k x x
                (let ((n ,n-procs))
                  (let ((id (gid)))
                    (let ((vp (provision id)))
                      (let ((vp (provision id)))
                        (let ((n-started (ref 1)))
                          (let ((n-done (ref 0)))
                            (fun (wc-switch sign)
                                 (let ((next-job ,(fai (term n-started))))
                                   (handle sign
                                           (stop-handler 
                                            (λ ()
                                              (if (<i next-job n-jobs)
                                                  (,(schedule-fiber (term wc-switch) 
                                                                    (term (λ () (job-fn next-job)))))
                                                  (if (=i ,(fai (term n-done)) n)
                                                      (k)
                                                      (forward (stop))))))
                                           (preempt-handler (λ (k)
                                                              (begin ,atomic-yield
                                                                     (run wc-switch k))))))
                              (begin
                                ,(dispatch-on (term vp) (term wc-switch))
                                (,(schedule-fiber (term wc-switch)
                                                  (term (λ () (job-fn 0)))))))))))))))))
  )
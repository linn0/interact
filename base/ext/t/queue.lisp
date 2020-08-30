

(deftest:deftest "basic queue tests" ()
  (let ((q (make-queue))
        (items (loop for i below 4096 collect i)))
    (dolist (i items)
      (queue-push q i)
      (assert (eql 0 (queue-peek q)))
      (assert (null (queue-cell-next (queue-tail q)))))
    (dolist (i items)
      (assert (eql i (queue-pop q)))
      (assert (or (not (queue-tail q))
                  (null (queue-cell-next (queue-tail q)))))
      (let ((next (queue-peek q)))
        (assert (or (not next)
                    (eql (1+  i) next)))))))


#+lispworks
(deftest:deftest "single writer/single-reader queue tests" ()
  (let* ((q (make-queue))
         (items (loop for i below 524288 collect i))
         (popped (list))
         (popper (mp:process-run-function
                  "queue popper" ()
                  (lambda ()
                    (loop
                     do (multiple-value-bind (value empty-p)
                            (queue-pop q :wait-p 10)
                          (when empty-p
                            (loop-finish))
                          (push value popped))))))
         (pusher (mp:process-run-function
                  "queue pusher" ()
                  (lambda ()
                    (dolist (i items)
                      (queue-push q i)
                      (sleep (random 0.0001)))))))
    (mp:process-wait "waiting for queue workers"
                     (let ((procs (list popper pusher)))
                       (lambda ()
                         (notany #'mp:process-alive-p procs))))
    (assert (equalp items
                    (sort popped #'<)))))


#+lispworks
(deftest:deftest "single writer/multi-reader queue tests" ()
  (let* ((q (make-queue))
         (items (loop for i below 524288 collect i))
         (pusher (mp:process-run-function
                  "queue pusher" ()
                  (lambda ()
                    (dolist (i items)
                      (queue-push q i)
                      (sleep (random 0.0001)))))))
    (loop
     repeat 8
     for x = (list (list))
     collect x into results
     collect (mp:process-run-function
              "queue popper" ()
              (let ((x x))
                (lambda ()
                  (loop
                   do
                   (multiple-value-bind (value empty-p)
                       (queue-pop q :wait-p 10)
                     (when empty-p
                       (loop-finish))
                     (push value (first x))))))) into procs
     finally (progn
               (mp:process-wait "waiting for queue workers"
                                (let ((procs (cons pusher procs)))
                                  (lambda ()
                                    (notany #'mp:process-alive-p procs))))
               (assert (equalp items
                               (sort (apply #'append (mapcar #'car results)) #'<)))))))


#+lispworks
(deftest:deftest "multi-writer/multi-reader queue tests" ()
  (let ((q (make-queue))
        (items (loop for i below 65536 collect i)))
    (loop
     repeat 8
     for x = (list (list))
     collect x into results
     collect (mp:process-run-function
              "queue popper" ()
              (let ((x x))
                (lambda ()
                  (loop
                   do
                   (multiple-value-bind (value empty-p)
                       (queue-pop q :wait-p 10)
                     (when empty-p
                       (loop-finish))
                     (push value (first x))))))) into procs
     collect (mp:process-run-function
              "queue pusher" ()
              (lambda ()
                (dolist (i items)
                  (queue-push q i)
                  (sleep (random 0.0001))))) into procs
     appending items into expected
     finally (progn
               (mp:process-wait "waiting for queue workers"
                                (lambda ()
                                  (notany #'mp:process-alive-p procs)))
               (assert (equalp (sort expected #'<)
                               (sort (apply #'append (mapcar #'car results)) #'<)))))))

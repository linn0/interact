
(in-package #:ccl)

(defun arrow-macro (init exps &optional >>-p some-p)
  (let ((exps (mapcar (lambda (exp)
                        (cond ((symbolp exp) `(,exp))
                              ((and (typep exp 'cons) (eq 'function (car exp)))
                               (if >>-p
                                   `(funcall (function ,(cadr exp)))
                                   `(->> (funcall (function ,(cadr exp))))))
                              ((and (typep exp 'cons) (eq 'lambda (car exp)))
                               (if >>-p
                                   `(funcall ,exp)
                                   `(->> (funcall ,exp))))
                              (t exp)))
                      exps)))
    (cond (some-p
           (let ((gblock (gensym)))
             `(block ,gblock
                ,(cadr
                  (let ((init `(or ,init (return-from ,gblock nil))))
                    (if >>-p
                        (reduce (lambda (e1 e2)
                                  `(or ,(append e2 (cons e1 nil)) (return-from ,gblock nil)))
                                (cons init exps))
                        (reduce (lambda (e1 e2)
                                  `(or (,(car e2) ,e1 ,@(cdr e2)) (return-from ,gblock nil)))
                                (cons init exps))))))))
          (>>-p (reduce (lambda (e1 e2) (append e2 (cons e1 nil))) (cons init exps)))
          (t (reduce (lambda (e1 e2) `(,(car e2) ,e1 ,@(cdr e2))) (cons init exps))))))

(defmacro -> (init &body exps) (arrow-macro init exps))
(defmacro ->> (init &body exps) (arrow-macro init exps t))
(defmacro some-> (init &body exps) (arrow-macro init exps nil t))
(defmacro some->> (init &body exps) (arrow-macro init exps t t))

(defmacro as-> (init var &body exps)
  `(let ((,var ,init))
     ,var
     ,@(loop for (exp next-exp) on exps
          collect (if next-exp `(setf ,var ,exp) exp))))

(defun cond-arrow-macro (init exps &optional >>-p)
  (let ((gvar (gensym)) (arrow (if >>-p '->> '->)))
    `(-> ,init
       ,@(loop for (pred form) on exps by #'cddr
            collect `(lambda (,gvar) (if ,pred (,arrow ,gvar ,form) ,gvar))))))

(defmacro cond-> (init &body exps) (cond-arrow-macro init exps))
(defmacro cond->> (init &body exps) (cond-arrow-macro init exps t))

;; (define-symbol-macro <> (error "do not use <> outside the scope of diamond-wand macros!"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '(
     ->
     ->>
     some->
     some->>
     as->
     cond->
     cond->>
     ) "CCL"
   )
  )

(in-package #:ext)

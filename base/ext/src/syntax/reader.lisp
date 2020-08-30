
(in-package #:ext)

(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +right-parens+ #\) )
(defconstant +comma+ #\,)
(defconstant +colon+ #\:)

(defun read-separator (stream)
 (let ((*readtable* (copy-readtable *readtable* nil)))
  (set-macro-character +comma+ (lambda (stream char)
                            (declare (ignore char stream))
                            'break))
  (read stream nil)))

(defun read-hash-table (stream subchar numarg)
  (declare (ignore subchar numarg))
  (let ((entry-list (read-delimited-list #\} stream t))
        (*readtable* (copy-readtable *readtable* nil)))
   (let
    ((pairs (loop for key = (pop entry-list)
                  for value = (pop entry-list)
                  while (not (null key))
                  collect (list key value)))
     (hash (gensym)))
    `(let
      ((,hash (make-hash-table :test #'equal)))
      ,@(mapcar
         (lambda (pair)
          `(setf (gethash ,(car pair) ,hash) ,(cadr pair)))
         pairs)
      ,hash))))

(defun print-hash-table (stream hash)
  (format stream "#{~{~{~S ~S~}~^ ~}}"
   (loop for key being the hash-keys of hash
         for value being the hash-values of hash
         collect (list key value))))

(defmethod print-object ((hash hash-table) stream)
  (print-hash-table stream hash))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (read-delimited-list +right-bracket+ stream t))

(defvar *previous-readtables* nil)
(defvar *previous-print-pprint-dispatch* nil)

(defmacro enable ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character +left-bracket+ 'read-left-bracket)
    (set-macro-character +right-bracket+ (get-macro-character +right-parens+ nil))
    (set-dispatch-macro-character #\# +left-brace+ #'read-hash-table)
    (set-macro-character +right-brace+ (get-macro-character +right-parens+ nil))
    (push *print-pprint-dispatch* *previous-print-pprint-dispatch*)
    (setq *print-pprint-dispatch* (copy-pprint-dispatch nil))
    (set-pprint-dispatch 'hash-table #'print-hash-table) ; depend on *print-pretty*
  ))

(defmacro disable ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))
    (setq *print-pprint-dispatch* (pop *previous-print-pprint-dispatch*))))

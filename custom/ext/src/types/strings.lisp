
(in-package #:ext)

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defun trim-left (s)
  "Remove whitespaces at the beginning of s. "
  (string-left-trim *whitespaces* s))

(defun trim-right (s)
  "Remove whitespaces at the end of s."
  (string-right-trim *whitespaces* s))

(defun trim (s)
  "Remove whitespaces at the beginning and end of s.
@begin[lang=lisp](code)
(trim \"  foo \") ;; => \"foo\"
@end(code)"
  (string-trim *whitespaces* s))

(defun concat (&rest strings)
  "Join all the string arguments into one string."
  (apply #'concatenate 'string strings))

(defun join (lst &key (separator ""))
  "Joins a list of strings (or other objects) in a string,
  delimited by \"separator\""
  (check-type lst list)
  (check-type separator string)
  (if lst
    (with-output-to-string (stream)
      (princ (first lst) stream)
      (dolist (el (rest lst))
        (write-string separator stream)
        (princ el stream)))
    ""))

(defun chars (string)
  "Returns a list with the chars in \"string\""
  (loop for c across string
    collect c))

(defun split (string &optional (separator #\space) &key (ignore-case nil))
  "Returns a list of substrings of string
  divided by separator. Separator can be a string or
  a character.
  Note: Two consecutive separators will be seen as
  if there were an empty string between them."
  (labels ((%split-by-char (string separator)
            (loop for i = 0 then (1+ j)
                  as j = (position separator string :start i :test (if ignore-case
                                                                    #'char-equal
                                                                    #'char=))
                  collect (subseq string i j)
                  while j))
           (%split-by-str (string separator)
             (loop for i = 0 then (+ j (length separator))
                   as j = (search separator string :start2 i :test (if ignore-case
                                                                    #'string-equal
                                                                    #'string=))
                   collect (subseq string i j)
                   while j)))
      (check-type string string)
      (cond ((typep separator 'character)
             (%split-by-char string separator))
            ((string= separator "") (chars string))
            ((typep separator 'string)
             (%split-by-str string separator))
            (t (error 'type-error :datum separator :expected-type 'string)))))

(defun starts-with-p (s start &key (ignore-case nil))
  "Return t if s starts with the substring 'start', nil otherwise."
  (when (>= (length s) (length start))
    (let ((fn (if ignore-case #'string-equal #'string=)))
      (funcall fn s start :start1 0 :end1 (length start)))))

(defun ends-with-p (s end &key (ignore-case nil))
  "Return t if s ends with the substring 'end', nil otherwise."
  (when (>= (length s) (length end))
    (let ((fn (if ignore-case #'string-equal #'string=)))
      (funcall fn s end :start1 (- (length s) (length end))))))

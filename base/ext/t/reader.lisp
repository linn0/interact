
(ext:enable)

(let ([x 1]) (1+ x))
#{:id 1 :name "let over lambda" :type "book"}

(-> 1 (+ 2 3) #'1+ 1+ (lambda (x) (+ x 1)) (1+)) ; => 10
(->> (list 1 2 3 4 5)
  (mapcar #'1+)
  (reduce #'+)) ; => 20
(-<> "abcdefghijklmnopqrstuvwxyz"
  (ppcre:scan-to-strings "j.*q" <>)
  (sort #'string>)
  (string-upcase :end 1)) ; => "Qponmlkj"
(-> (get-some-webpage-uri id)
  drakma:http-request
  babel:octets-to-string
  json->list)

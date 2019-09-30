
(in-package :datapump)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :html))

(defun player (session resp)
  "Generate the page."
  (declare (ignore session))

  ;; set the response type
  (push-text-content-type resp)

  (let ((url "xfplay://dna=AZeZEHHZDZbgAHEeEdIbAwL5Ewa4mwyfmdH5Dwx1mGudEeeYAwueEa|dx=1712865868|mz=复仇者联盟.mkv"))
    ;; build the page and return it
    (http:http-ok resp
             (html-render (<html>
                           (<head> (<title> "player"))
                           (<body>
                            (<object> :id "Xfplay" :name "Xfplay" :width "900" :height "550" :classid "clsid:E38F2429-07FE-464A-9DF6-C14EF88117DD"
                              (<param> :name "URL" :value url)
                              (<param> :name "Status" :value "1")
                              (<embed> :type "application/xfplay-plugin" :id "Xfplay2" :name "Xfplay2" :param_url url :param_status "1" 
                                :width "900" :height "550"))))))))

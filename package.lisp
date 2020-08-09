(defpackage :cl-bff
  (:use :cl
        #+sbcl :sb-alien)
  (:export :execute-program))

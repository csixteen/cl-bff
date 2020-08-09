(defpackage :cl-bff
  (:use :cl
        #+sbcl :sb-alien)
  (:export :execute-program
           :sanitize
           :parse-arguments
           :print-help))

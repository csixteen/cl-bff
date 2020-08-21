(uiop:define-package :cl-bff
  (:use :cl
        :cl-bff.args
        :cl-bff.termios)
  (:export :execute-program
           :sanitize))

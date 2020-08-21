(uiop:define-package :cl-bff.brainfuck
  (:use :cl
        :cl-bff.args
        :cl-bff.termios)
  (:export :sanitize
           :is-valid-operator
           :execute-program
           :sanitize))

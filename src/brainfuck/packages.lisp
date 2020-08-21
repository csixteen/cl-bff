(uiop:define-package :cl-bff.brainfuck
  (:use :cl
        :cl-bff.args
        :cl-bff.termios)
  (:export :execute-program
           :sanitize))

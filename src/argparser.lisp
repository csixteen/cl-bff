; Copyright 2020 Pedro Rodrigues
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(in-package :cl-bff)


(defun is-flag? (arg)
  (string-equal "-" (subseq arg 0 1)))


(defun print-help (&key (s *error-output*) (quit t))
  (progn
    (format s ":: cl-bff (Brainfuck interpreter written in Lisp)")
    (format s "~%~%Usage: cl-bff <FILENAME> [[-h] | [--mem-size <number>]]~%~%")
    (format
      s
      "Options:

-h                   Prints this help menu
--mem-size <number>  The number of memory cells available during runtime.~%")
    (when quit
      (sb-ext:quit))))


(defun read-numeric-argument (arg)
  "Tries to parse a string into an integer. Instead of accepting
  junk in the string or erroring, this will simply return `nil`."
  (when (null (find-if-not #'digit-char-p arg))
    (parse-integer arg)))


(defun parse-arguments (args)
  (cond ((null args) nil)
        ((string-equal (car args) "-h") (print-help))
        ((string-equal (car args) "--mem-size")
         ; Do better error checking here
         (cons (list "mem-size" (read-numeric-argument (cadr args)))
               (parse-arguments (cddr args))))
        ((is-flag? (car args))
         (format *error-output* "ERROR: unknown option ~S~%" (car args))
         (print-help))
        (t (cons (list "file-name" (car args))
                 (parse-arguments (cdr args))))))


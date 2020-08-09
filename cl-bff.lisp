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

(require :asdf)
(require :cl-bff)


(defconstant *default-memory-size* 300)


(defun run-code (code mem-size)
  "Takes a sequence that represents potential Brainfuck code and
  runs it."
  (let* ((sane-code (cl-bff:sanitize code))
         (len (length sane-code)))
    (cl-bff:execute-program
      (make-array len :initial-contents sane-code)
      (make-array mem-size :initial-element 0)
      nil
      0
      0)))


(defun print-help (&key (s *error-output*) (quit t))
  (progn
    (format s "~%Usage: ~S [[--help] | [--mem-size <number>]]~%"
            (car sb-ext:*posix-argv*))
    (when quit
      (sb-ext:quit))))


(defun read-numeric-argument (arg)
  "Tries to parse a string into an integer. I'm not interested
  in allowing junk in the string, which is why I'm defining
  this method to return nil instead of crashing with junk."
  (cond ((null (find-if-not #'digit-char-p arg))
         (parse-integer arg))
        (t nil)))


(defun parse-arguments (args)
  (cond ((or (null args) (string-equal (car args) "-h"))
         (print-help))
        ((string-equal (car args) "--mem-size")
         (let ((mem-size (read-numeric-argument (cadr args))))
           (cond ((null mem-size)
                  (print-help))
                 (t mem-size))))
        ((string-equal (car args) "--help")
         (print-help :s *standard-output*))
        (t (format *error-output* "ERROR: unknown option ~S~%" (car args))
           (print-help))))

(defun main()
  (let ((mem-size (or (parse-arguments (cdr sb-ext:*posix-argv*))
                      *default-memory-size*)))
    (format t "Memory size: ~A~%" mem-size)))


;;;; ------------------------------------------------


(save-lisp-and-die
  "bin/cl-bff"
  :purify t
  :executable t
  :toplevel #'main)

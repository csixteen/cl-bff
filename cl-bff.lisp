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

(defun run-code (code &key (mem-size *default-memory-size*))
  "Takes a sequence that represents potential Brainfuck code and
  runs it."
  (let* ((sane-code (sanitize code))
         (len (length sane-code)))
    (execute-program
      (make-array len :initial-contents sane-code)
      (make-array mem-size :initial-element 0)
      nil
      0
      0)))

(defun print-help (&optional (s *error-output*))
  (format s "~%Usage: ~S [[--help] | [--mem-size <int>]]~%"
          (car sb-ext:*posix-argv*)))

(defun parse-arguments (args)
  (cond ((null args) nil)
        ((string-equal (car args) "--mem-size")
         (cadr args)) ; Check for parsable integer
        ((string-equal (car args) "--help")
         (print-help *standard-output*)
         (sb-ext:quit))
        (t (format *error-output* "ERROR: unknown option ~S~%" (car args))
           (print-help)
           (sb-ext:quit))))


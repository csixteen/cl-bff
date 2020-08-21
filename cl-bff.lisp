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


(declaim (optimize (safety 3) (debug 1) (speed 2)))


(require :asdf)
(require :cl-bff)
(require :cl-bff.args)


(defconstant *default-memory-size* 300)


(defun read-code-from-file (filename)
  "Attempts to read the Brainfuck source code from a file, given
  a path."
  (let ((path (uiop:probe-file* filename)))
    (cond ((null path)
           (format *error-output* "ERROR: file ~S doesn't exist~%" filename)
           (sb-ext:quit))
          (t (uiop:read-file-string filename)))))


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


(defun get-arg (argname args)
  (cadr (find argname args :test #'string-equal :key #'car)))


(defun main()
  (let* ((params (cl-bff.args:parse-arguments (cdr sb-ext:*posix-argv*)))
         (filename (get-arg "file-name" params))
         (mem-size (or (get-arg "mem-size" params) *default-memory-size*)))
    (if (null filename)
      (cl-bff.args:print-help)
      (run-code (read-code-from-file filename) mem-size))))


;;;; ------------------------------------------------


(save-lisp-and-die
  "bin/cl-bff"
  :purify t
  :executable t
  :toplevel #'main)

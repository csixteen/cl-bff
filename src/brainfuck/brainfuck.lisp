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

(in-package :cl-bff.brainfuck)


(defparameter +brainfuck-operators+ '(#\< #\> #\[ #\] #\- #\+ #\, #\.))


(defun >1- (value &optional (max-val most-positive-fixnum))
  (if (zerop value)
    max-val
    (1- value)))


(defun <1+ (value &optional (max-val most-positive-fixnum))
  (if (= value max-val)
    0
    (1+ value)))


(defun op-shl (code mem stack cursor pc)
  "Shifts the memory cursor one position to the left."
  (values mem stack (>1- cursor) (1+ pc)))


(defun op-shr (code mem stack cursor pc)
  "Shifts the memory cursor one position to the right."
  (values mem stack (<1+ cursor) (1+ pc)))


(defun op-loop-start (code mem stack cursor pc)
  "Advances the program counter to the next instruction if the
  value of the current memory cell is greater than zero. If not,
  it advances the program counter to the matching `]`."
  (cond ((> (aref mem cursor) 0)
         (values mem (cons pc stack) cursor (1+ pc)))
        (t (let ((i (<1+ pc))
                 (skipped 0))
             (loop (when (>= i (length code))
                     (return (values mem stack cursor i)))
                   (case (aref code i)
                     (#\[ (psetq skipped (1+ skipped)))
                     (#\] (cond ((zerop skipped)
                                 (psetq i (1+ i))
                                 (return (values mem stack cursor i)))
                                (t (psetq skipped (1- skipped))))))
                   (psetq i (1+ i)))))))


(defun op-loop-end (code mem stack cursor pc)
  "Advances the program counter to the next instruction if
  the value of the current memory cell is zero. If not, it
  moves the program counter back to the matching `[`."
  (cond ((zerop (aref mem cursor))
         (values mem (cdr stack) cursor (1+ pc)))
        (t (let ((offset (1+ (- (car stack) pc))))
             (values mem stack cursor (+ pc offset))))))


(defun op-decrement (code mem stack cursor pc)
  "Decrements the value of the current memory cell by 1."
  (when (< cursor (length mem))
    (setf (aref mem cursor) (>1- (aref mem cursor) 255)))
  (values mem stack cursor (1+ pc)))


(defun op-increment (code mem stack cursor pc)
  "Increments the value of the current memory cell by 1."
  (when (< cursor (length mem))
    (setf (aref mem cursor) (<1+ (aref mem cursor) 255)))
  (values mem stack cursor (1+ pc)))


(defun op-read (code mem stack cursor pc)
  (when (< cursor (length mem))
    (setf (aref mem cursor) (read-single-byte)))
  (values mem stack cursor (1+ pc)))


(defun op-write (code mem stack cursor pc)
  "Writes to STDOUT the ASCII character corresponding to the 
  integer in the current memory cell."
  (when (< cursor (length mem))
    (format t "~A" (code-char (aref mem cursor))))
  (values mem stack cursor (1+ pc)))


;;;; ----------------------------------------------
;;;; Helpers


(defun action-op (c)
  "Gets the action for the operator represented by the character
  `c`, should there exist one."
  (case c
    ((#\<) 'op-shl)
    ((#\>) 'op-shr)
    ((#\[) 'op-loop-start)
    ((#\]) 'op-loop-end)
    ((#\+) 'op-increment)
    ((#\-) 'op-decrement)
    ((#\,) 'op-read)
    ((#\.) 'op-write)
    (otherwise "unknown operator")))


(defun is-valid-operator (c)
  "Indicates whether char `c` represents a valid Brainfuck operator
  or not."
  (find c +brainfuck-operators+ :test #'char-equal))


(defun sanitize (code)
  "Takes Brainfuck code and removes any character that is not
  a valid operator."
  (remove-if-not #'is-valid-operator code))


;;;; ------------------------------------------------------
;;;; Program and instruction execution

(defun execute-next-instruction (code mem stack cursor pc)
  "Executes the next instruction in the code indexed by
  the program counter."
  (let ((c (aref code pc)))
    (funcall (action-op c) code mem stack cursor pc)))


(defun execute-program (code mem stack cursor pc)
  "Executes the Brainfuck code, using mem as the program
  memory, cursor as the memory cursor and pc as the
  program counter. It will stop when the program counter
  has reached the end of the code."
  (let ((m mem)
        (s stack)
        (c cursor)
        (p pc))
    (loop (when (>= p (length code)) (return))
          (multiple-value-bind (next-m next-s next-c next-p)
            (execute-next-instruction code m s c p)
            (psetq m next-m
                   s next-s
                   c next-c
                   p next-p)))))

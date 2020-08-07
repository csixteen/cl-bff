(defconstant *default-memory-size* 30000)

;;;; ----------------------------------------------
;;;; Brainfuck Operators

(setf (get '<     'operators) 'op-shl)
(setf (get '>     'operators) 'op-shr)
(setf (get '[     'operators) 'op-loop-start)
(setf (get ']     'operators) 'op-loop-end)
(setf (get '-     'operators) 'op-decrement)
(setf (get '+     'operators) 'op-increment)
(setf (get 'comma 'operators) 'op-read)
(setf (get 'dot   'operators) 'op-write)


(defun op-shl (code mem stack cursor pc)
  (values mem stack (1- cursor) pc))


(defun op-shr (code mem stack cursor pc)
  (values mem stack (1+ cursor) pc))


(defun op-loop-start () 'LOOP-START)


(defun op-loop-end (code mem stack cursor pc)
  (cond ((zerop (aref mem cursor))
         (values mem (cdr stack) (1+ pc)))
        (t (let ((offset (1+ (- (car stack) pc))))
             (values mem stack cursor (+ pc offset))))))


(defun op-decrement (code mem stack cursor pc)
  (prog
    (setf (aref mem cursor) (1- (aref mem cursor)))
    (values mem stack cursor pc)))


(defun op-increment (code mem stack cursor pc)
  (prog
    (setf (aref mem cursor) (1+ (aref mem cursor)))
    (values mem stack cursor pc)))


(defun op-read () 'READ)


(defun op-write (code mem stack cursor pc)
  (progn
    (format t "~A" (int-char (aref mem cursor)))
    (values mem stack cursor pc)))


;;;; ----------------------------------------------
;;;; Helpers

(defun char-to-symbol (c)
  (cond ((eql c #\,) 'comma)
        ((eql c #\.) 'dot)
        (t (intern (string c)))))


(defun action-op (c)
  "Gets the action for the operator represented by the character
  `c`, should there exist one."
  (get (char-to-symbol c) 'operators))


(defun is-valid-operator (c)
  "Indicates whether char `c` represents a valid Brainfuck operator
  or not."
  (not (null (action-op (char-to-symbol c)))))


(defun sanitize (code)
  "Takes Brainfuck code and removes any character that is not
  a valid operator."
  (remove-if-not 
    #'(lambda (c) (not (is-valid-operator c)))
    code))


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
  (unless (>= pc (length code))
    (multiple-value-bind (m s c p)
      (execute-next-instruction code mem stack cursor pc)
      (execute-program code m s c p))))


;;; ---------------------------------------------------------
;;; run

(defun run (code &key (mem-size *default-memory-size*))
  (let ((len (length code)))
    (execute-program
      (make-array len :initial-contents code)
      (make-array mem-size :initial-element 0)
      nil
      0
      0)))
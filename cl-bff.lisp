(defconstant *default-memory-size* 30000)

;;;; Brainfuck Operators

(setf (get '<     'operators) 'op-shl)
(setf (get '>     'operators) 'op-shr)
(setf (get '[     'operators) 'op-loop-start)
(setf (get ']     'operators) 'op-loop-end)
(setf (get '-     'operators) 'op-decrement)
(setf (get '+     'operators) 'op-increment)
(setf (get 'comma 'operators) 'op-read)
(setf (get 'dot   'operators) 'op-write)

(defun op-shl () 'SHIFT-LEFT)

(defun op-shr () 'SHIFT-RIGHT)

(defun op-loop-start () 'LOOP-START)

(defun op-loop-end () 'LOOP-END)

(defun op-decrement () 'DECREMENT)

(defun op-increment () 'INCREMENT)

(defun op-read () 'READ)

(defun op-write () 'WRITE)

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

(defun execute-next-instruction (code mem cursor pc)
  (let ((c (aref code pc)))
    (funcall (action-op c) code mem cursor pc)))

(defun execute-program (code mem cursor pc)
  (unless (>= pc (length code))
    (multiple-value-bind (m c p)
      (execute-next-instruction code mem cursor pc)
      (execute-program code m c p))))


;;; --------------------------------------------------------
;;; sanitize

(defun sanitize (code)
  "Takes Brainfuck code and removes any character that is not
  a valid operator."
  (remove-if-not 
    #'(lambda (c) (not (is-valid-operator c)))
    code))


;;; ---------------------------------------------------------
;;; Main


(defun main(code)
  (let ((len (length code)))
    (execute-program
      (make-array len :initial-contents code)
      (make-array *default-memory-size* :initial-element 0)
      0
      0)))

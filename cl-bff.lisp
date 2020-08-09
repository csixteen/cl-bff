;;;; FFI to access termios

#+sbcl
(define-alien-type nil
  (struct termios
          (c_iflag unsigned-int)
          (c_oflag unsigned-int)
          (c_cflag unsigned-int)
          (c_lflag unsigned-int)
          (c_cc (array unsigned-char 20))
          (c_ispeed int)
          (c_ospeed int)))

(declaim (inline tcgetattr))
(define-alien-routine "tcgetattr" int
                      (fd int)
                      (term (* (struct termios))))

(declaim (inline tcsetattr))
(define-alien-routine "tcsetattr" int
                      (fd int)
                      (action int)
                      (term (* (struct termios))))


;;;; ---------------------------------------------


(defconstant *default-memory-size* 300)

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
  "Shifts the memory cursor one position to the left."
  (values mem stack (1- cursor) (1+ pc)))


(defun op-shr (code mem stack cursor pc)
  "Shifts the memory cursor one position to the right."
  (values mem stack (1+ cursor) (1+ pc)))


(defun op-loop-start (code mem stack cursor pc)
  "Advances the program counter to the next instruction if the
  value of the current memory cell is greater than zero. If not,
  it advances the program counter to the matching `]`."
  (cond ((> (aref mem cursor) 0)
         (values mem (cons pc stack) cursor (1+ pc)))
        (t (let ((i (1+ pc))
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
  (progn
    (setf (aref mem cursor) (1- (aref mem cursor)))
    (values mem stack cursor (1+ pc))))


(defun op-increment (code mem stack cursor pc)
  "Increments the value of the current memory cell by 1."
  (progn
    (setf (aref mem cursor) (1+ (aref mem cursor)))
    (values mem stack cursor (1+ pc))))


(defun op-read () 'READ)


(defun op-write (code mem stack cursor pc)
  "Writes to STDOUT the ASCII character corresponding to the 
  integer in the current memory cell."
  (progn
    (format t "~A" (code-char (aref mem cursor)))
    (values mem stack cursor (1+ pc))))


;;;; ----------------------------------------------
;;;; Helpers

(defun read-char-no-echo-cbreak (&optional (stream *query-io*))
  (with-alien ((old (struct termios))
               (new (struct termios)))
    (let ((e0 (unix-tcgetattr 0 old))
          (e1 (unix-tcgetattr 0 new))
          (bits (logior tty-icanon tty-echo tty-echoe
                        tty-echok tty-echonl)))
      (declare (ignorable e0 e1)) ;[probably should test for error here]
      (unwind-protect
           (progn
             (setf (slot new 'c-lflag) (logandc2 (slot old 'c-lflag) bits))
             (setf (deref (slot new 'c-cc) vmin) 1)
             (setf (deref (slot new 'c-cc) vtime) 0)
             (unix-tcsetattr 0 tcsadrain new)
             (read-char stream))
        (unix-tcsetattr 0 tcsadrain old)))))


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

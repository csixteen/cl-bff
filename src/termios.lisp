(in-package :cl-bff)

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

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

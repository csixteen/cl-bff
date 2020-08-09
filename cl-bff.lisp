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


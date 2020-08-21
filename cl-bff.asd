(asdf:defsystem :cl-bff/termios
  :description "FFI utilities for termios"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:uiop)
  :pathname "src/termios/"
  :serial t
  :components ((:file "packages")
               (:file "termios")))


(asdf:defsystem :cl-bff/args
  :description "Utilities for parsing command-line arguments"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:uiop)
  :pathname "src/argparser/"
  :serial t
  :components ((:file "packages")
               (:file "argparser")))


(asdf:defsystem :cl-bff/brainfuck
  :description "Utilities to parse and execute Brainfuck code"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:uiop :cl-bff/args :cl-bff/termios)
  :pathname "src/brainfuck/"
  :serial t
  :components ((:file "packages")
               (:file "brainfuck")))


(asdf:defsystem :cl-bff
  :description "Brainfuck interpreter."
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:uiop cl-bff/args :cl-bff/brainfuck)
  :serial t
  :components ((:file "cl-bff")))

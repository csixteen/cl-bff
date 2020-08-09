(asdf:defsystem :cl-bff
  :name "cl-bff"
  :description "Brainfuck interpreter written in Common Lisp"

  :author "Pedro Rodrigues <csixteen@protonmail.com>"

  :license "MIT"
  :version "0.1.0"

  :depends-on (
    :lisp-unit2
  )

  :components ((:file "package")

               (:module "src"
                :depends-on ("package")
                :serial t
                :components ((:file "termios")
                             (:file "argparser")
                             (:file "brainfuck")))))

.PHONY: bin

bin:
	@mkdir -p bin
	@sbcl \
		--eval '(and (load "cl-bff.asd") (asdf:load-system :cl-bff) (load "cl-bff.lisp"))' \
		--quit \
		--disable-debugger

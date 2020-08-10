.PHONY: bin
bin:
	@mkdir -p bin
	@sbcl                                                           \
		--eval '(and (load "cl-bff.asd") (load "cl-bff.lisp"))' \
		--quit                                                  \
		--disable-debugger

.PHONY: test
test:
	@sbcl                           \
		--noinform              \
		--load test-runner.lisp \
		--quit                  \
		--disable-debugger

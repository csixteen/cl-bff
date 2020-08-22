.PHONY: bin
bin:
	@mkdir -p bin
	@sbcl                                                                  \
		--eval '(and (load "cl-bff.asd") (asdf:load-system "cl-bff"))' \
		--quit                                                         \
		--disable-debugger

.PHONY: unit
unit:
	@sbcl                                                                  \
		--noinform                                                     \
		--eval '(and (load "cl-bff.asd") (load "t/test-runner.lisp"))' \
		--quit                                                         \
		--disable-debugger

.PHONY: test

FILES := $(shell find ./t/examples/ -name "*.bf" -exec basename -s .bf {} \;)
JOBS := $(addprefix job,${FILES})

test: bin ${JOBS} ; @echo "[$@] finished!"

${JOBS}: job%: ; ./bin/cl-bff t/examples/$*.bf

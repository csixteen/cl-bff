.PHONY: bin
bin:
	@mkdir -p bin
	@sbcl                                                           \
		--eval '(and (load "cl-bff.asd") (load "cl-bff.lisp"))' \
		--quit                                                  \
		--disable-debugger

.PHONY: unit
unit:
	@sbcl                           \
		--noinform              \
		--load test-runner.lisp \
		--quit                  \
		--disable-debugger

.PHONY: test

FILES := $(shell find ./tests/ -name "*.bf" -exec basename -s .bf {} \;)
JOBS := $(addprefix job,${FILES})

test: bin ${JOBS} ; @echo "[$@] finished!"

${JOBS}: job%: ; ./bin/cl-bff tests/$*.bf

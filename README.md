# cl-bff (WORK IN PROGRESS)

Yet another Brainfuck interpreter, this time written in Common Lisp.

# Dependencies

This has only been tested with [SBCL](http://sbcl.org). Also, you may want to install [Quicklisp](https://www.quicklisp.org/beta/), if you still haven't.

- [lisp-unit2](http://quickdocs.org/lisp-unit2/)
- [ASDF](https://common-lisp.net/project/asdf/)

# Compiling
In order to compile _cl-bff_ into a single binary, you just need to call `make`:

```
$ make bin
...
;;; Computing Hangul syllable namesSTYLE-WARNING:
   Defining (*DEFAULT-MEMORY-SIZE*) as a constant, even though the name follows
the usual naming convention (names like *FOO*) for special variables
[undoing binding stack and other enclosing state... done]
[performing final GC... done]
[defragmenting immobile space... (fin,inst,fdefn,code,sym)=1502+1230+21244+22074+26917... done]
[saving current Lisp image into bin/cl-bff:
writing 0 bytes from the read-only space at 0x20000000
writing 736 bytes from the static space at 0x20100000
writing 49217536 bytes from the dynamic space at 0x1000000000
writing 2232320 bytes from the immobile space at 0x20200000
writing 14204928 bytes from the immobile space at 0x21a00000
done]
$ ls bin/
cl-bff
$
```

# TODO

- Implement the "read" operator (`,`).
- Read Brainfuck source code from file..
- Test with more complex Brainfuck code.
- Write unit tests.

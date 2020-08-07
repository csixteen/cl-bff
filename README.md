# cl-bff

Yet another Brainfuck interpreter, this time written in Common Lisp.

# Testing

This is still work in progress. The easiest way to test it is to load the file in your favourite Lisp REPL:
```
$ sbcl

-- snip --

CL-USER(1): (load "cl-bff.lisp")

-- snip --

CL-USER(2): (run ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>+++++[<++++++++>-]<++++.[-]>++++++++[<++++>-]<.>+++++++++++[<++++++++>-]<-.--------.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++.")
Hello, world!
NIL
CL-USER(3):
```

# TODO

- Implement the "read" operator (`,`).
- Read contents from file and feed the contents to `(run (sanitize contents))`.
- Read memory size from argv.
- Build a single binary.
- Test with more complext Brainfuck code.
- Write unit tests.

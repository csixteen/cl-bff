# cl-bff

Yet another Brainfuck interpreter, this time written in Common Lisp. Use with caution, this is an ongoing exercise for me to learn Lisp. It successfully interprets all the Brainfuck code in `tests/` (includig 99 Bottles of beer), which makes me very happy!

# Dependencies

This has only been tested with [SBCL](http://sbcl.org). Also, you may want to install [Quicklisp](https://www.quicklisp.org/beta/), if you still haven't.

- [lisp-unit](https://www.cliki.net/lisp-unit)
- [ASDF](https://common-lisp.net/project/asdf/)

# Compiling
In order to compile _cl-bff_ into a single binary, you just need to call `make bin`:

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

# Running
After you've compiled the code, you can simply run `bin/cl-bff`:

```
$ ./bin/cl-bff tests/hello.bf
Hello, world!
$ ./bin/cl-bff tests/hello.bf --mem-size 30
Hello, world!
```

# Testing
You can run the unit tests with `make test` target:
```
$ make test
TEST-SANITIZE: 1 assertions passed, 0 failed.

TEST-IS-VALID-OPERATOR: 2 assertions passed, 0 failed.

Unit Test Summary
 | 3 assertions total
 | 3 passed
 | 0 failed
 | 0 execution errors
 | 0 missing tests

$
```

# TODO

- Improve error handling. For example, things will fail miserably if we invoke `cl-bff --mem-size` without a mem size. Should be easy to fix though.
- Perhaps put the tests in a separate package, so that they don't get added to the binary.

# LICENSE

See [LICENSE](https://github.com/csixteen/cl-bff/blob/master/LICENSE).

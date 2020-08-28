# cl-bff

Yet another Brainfuck interpreter, this time written in Common Lisp.

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
$ ./bin/cl-bff t/examples/hello.bf
Hello, world!
$ ./bin/cl-bff t/examples/hello.bf --mem-size 30
Hello, world!
```

# Testing
You can test the code against all the Brainfuck source files in `tests/` by running the following `make` target:

```
$ make test
-- snip --
1 Bottle of beer on the wall
1 Bottle of beer
Take one down and pass it around
0 Bottles of beer on the wall

./bin/cl-bff t/examples/hello.bf
Hello, world!
./bin/cl-bff t/examples/hello4.bf
Hello World!
./bin/cl-bff t/examples/hello5.bf
Hello World!
[test] finished!
$
```

# Running unit tests
You can run the unit tests with `make unit` target:
```
$ make unit
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

Still need to add more, though.

# TODO

- Increate code coverage.

# Contributing

If you find any bug or want to improve the code, feel free to open a PR!

# LICENSE

See [LICENSE](https://github.com/csixteen/cl-bff/blob/master/LICENSE).

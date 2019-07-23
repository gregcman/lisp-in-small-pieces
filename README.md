##  The Bytecode compiler from Lisp In Small Pieces in Common Lisp

This is a bytecode compiler for scheme in Common Lisp. 

#### Install 

1. Install ![scheme2common-lisp](https://github.com/gmasching/scheme2common-lisp)

2. Clone this project into your ASDF system

3. Load the ASDF system with quicklisp or ASDF: ```(asdf::load-op :lisp-in-small-pieces)```

#### Evaluate a Scheme form

```
(lisp::%eval 
  '((lambda (fact)
      (begin (set! fact (lambda (n)
        (if (< n 2) 1
            (* n (fact (- n 1))))))
       (fact 5)))
    'fact))
```

#### Running the REPL
```(lisp::scheme7d)```

#### Running the tests for the bytecode compiler and interpreter
```(lisp::test-scheme7d)```

# Racket Interpreter

Racket interpreter made by Haskell and [Happy](https://www.haskell.org/happy/)

## Installation

1. make sure you have already installed Haskell.

2. clone this repository.

```
$ git clone https://github.com/hideUW/racket_interpreter.git
```

## Getting started

1. Use below command to start Racket Interpreter.

```
$ runhaskell OctopusInterpreter.hs
```

2. Type random Racket codes.

```
>>> (+ 1 1)
2
>>> (define x 2)
>>> (* x 3)
6
>>> ((lambda (x y) ((lambda (y) (+ x y)) 10)) 1 2)
11
>>> (letrec ((zero? (lambda (x) (equal? x 0)))(even? (lambda (x) (if (zero? x) #t (odd? (- x 1)))))(odd?  (lambda (x) (if (zero? x) #f (even? (- x 1)))))(mod2 (lambda (x) (if (even? x) 0 1))))(cons (mod2 5) (cons (mod2 6) '())))
(1 0)
>>>
```

3. You can exit by just typing return.

## In case you edit parser

1. Edit OctoParser.y

2. Generate Haskell lexer file by using below command.

```
$ Happy OctoParser.y
```

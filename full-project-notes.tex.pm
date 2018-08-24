#lang pollen
◊(require "latex-commands.rkt")
◊(define p 3)
◊(define q 5)
◊(define n (* p q))
◊(define generator (add1 n))
◊(define modulus (* n n))
◊(define key-value 4)
◊(define public-key 
  (ensure-math 
    (format "(~a, ~a)"
            generator
            n)))
◊(define client (ensure-math "C"))
◊(define client-index "i")
◊(define current-server-index (ensure-math "h"))

◊title{What Does an RMS "Machine" Look Like?}

There is a set of inputs.

The inputs are fixed and unchanging throughout the duration of any
program that's run on an RMS machine.

An input ◊|input|, has a value, ◊${x}, and is created with a secret key,
◊|key|.

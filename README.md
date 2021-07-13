# Layered interpreters.

Here is the microcode language, it emulates datapath level
operations. It's a small language that supports a limited set of
commands that mutate the state of a virtual BETA cpu with ram.
=======

```lisp
(add-instruction '(ADD RA RB RC) 'OP #b100000                 
                 "The contents of register Ra are added to the
                  contents of register Rb and the 32-bit sum is
                  written to Rc. This instruction computes no carry or
                  overflow information. If desired then this can be
                  computed through explicit compare instructions."

                 '((inc-pc)
                   (set-reg rc (+ (reg ra) (reg rb)))))
```

Here are some of the microcode commands.  

`inc-pc` increments the program counter, 
`(reg ra)` selects register `ra` from the register file
`(set-var)` can be used for naming variables
`(set-mem)` is used to set RAM locations

The languages can also handle expressions:  

`(+ a b)` and other arithmetic operations emulate the ALU

The machinery is an evalulator like the ones found in SICP

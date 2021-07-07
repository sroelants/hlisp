# hlisp

Learning about parser combinators by implementing a tiny lisp.

## How to use
No REPL yet. The Main module exports a utility `interpret` function that will read in a lisp string and evaluate it.

## Progress
- [x] Numbers (only integers are supported for now)
- [x] Strings
- [x] Bools (denoted `#t` and `#f`)
- [x] Lambdas (`(lambda x (+ x x))`)
- [x] Let bindings (`(let x 5 (+ x x))`)
- [x] Conditionals (`(if (= (+ 2 2) 5) "Big Brother won" "Still the captain of my soul")`)
- [ ] Quoting (hurts my brain) (`(quote (1 2 3))`)
- [ ] REPL 

## Standard library
hlisp supports a vast standard library:
- [x] Integer arithmetic: `+`, `-`, `*`
- [x] Equality testing: `=`

What else were you expecting?

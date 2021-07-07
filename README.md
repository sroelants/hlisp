# hlisp

Learning about parser combinators by implementing a tiny lisp.

## How to use
No REPL yet. The Main module exports a utility `interpret` function that will read in a lisp string and evaluate it.

## Progress
- [✓] Numbers (only integers are supported for now)
- [✓] Strings
- [✓] Bools (denoted `#t` and `#f`)
- [✓] Lambdas (`(lambda x (+ x x))`)
- [✓] Let bindings (`(let x 5 (+ x x))`)
- [✓] Conditionals (`(if (= (+ 2 2) 5) "Big Brother won" "Still the captain of my soul")`)
- [ ] Quoting (hurts my brain) (`(quote (1 2 3))`)
- [ ] REPL 

## Standard library
hlisp supports a vast standard library:
- [ ] Integer arithmetic: `+`, `-`, `*`
- [ ] Equality testing: `=`

What else were you expecting?

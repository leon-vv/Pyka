# Pyka Programming Language

The Pyka language is a Lisp language that uses _first class environments_ to elegantly combined dynamic and lexical scoping. Common Lisp constructs like 'lambda' or 'defmacro' do not have a special status in this language but can be constructed from the Pyka language primitives. The function naming convention is based on the RSR7 (small) Scheme specification.


## Philosophy


The idea behind the language is to expose the current environment of any expression being evaluated. Since we're implementing a Lisp language the environment can most easily be defined as a list of hash tables. Whenever a function is called a new hash table (holding the function arguments) is pushed onto the list, making the language inherently dynamically scoped. The current environment is always accessible through the special `current-env` callable, which returns the list of hash tables representing the environment. It turns out that such direct access to the environment, combined with the power of [Fexpressions](https://en.wikipedia.org/wiki/Fexpr), and liberal use of the `eval` function allows us to implement [lexical scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope)! 

This makes Pyka, as far as I know, the first language in which lexical scoping can be implemented as a library. Furthermore powerful programming constructs like [macros](https://gigamonkeys.com/book/macros-defining-your-own.html) can also be implemented directly without being part of the core language.


## Usage 

To run  a REPL use:

```
pip3 install parsec
python3 ./src/bootstrap.py
```

This language is a work in progress so not currently very usable. I currently don't have much time to work on it actively.  Please see `/docs` for programming constructs that will eventually be supported. 

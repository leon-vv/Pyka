# Pyka Programming Language

The Pyka language is a Lisp language that uses 'first class environments' to elegantly combined dynamic and lexical scoping. Common Lisp constructs like 'lambda' or 'defmacro' do not have a special status in this language but can be constructed from the Pyka language primitives. To run  a REPL use:

```
pip3 install parsec
python3 ./src/bootstrap.py
```

This language is a work in progress so not currently very usable. Please see `/docs` for programming constructs that will eventually be supported.

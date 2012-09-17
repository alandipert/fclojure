# fclojure

![fclojure](https://dl.dropbox.com/u/12379861/fclojurelogo.png "fclojure logo")

fclojure is an interpreter written in Clojure for a small
Clojure-inspired language that supports FEXPRs.  A FEXPR is a lambda
that does not evaluate its arguments.  In fclojure, FEXPRs can be
created with the `ffn` special form.

fclojure is **experimental** and **in continual flux**, so don't try
to use it for anything boring.

## Some Reading

* [AIM 453 "Art of the Interpreter"](ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-453.pdf)
* [Fexpr on Wikipedia](http://en.wikipedia.org/wiki/Fexpr)
* [Kernel](http://web.cs.wpi.edu/~jshutt/kernel.html)
* [Fexprs as the basis of Lisp function application; or, $vau: the ultimate abstraction](http://www.wpi.edu/Pubs/ETD/Available/etd-090110-124904/)
* [Special Forms in Lisp](http://www.nhplace.com/kent/Papers/Special-Forms.html)

## License

Copyright © 2012 Alan Dipert

Distributed under the Eclipse Public License, the same as Clojure.
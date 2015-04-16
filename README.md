The main purpose of this project is to provide a way to incrementally migrate a
large Python codebase onto a statically typed language (Purescript) while
still providing Python interop.  If you maintain a large Python codebase you
probably understand why this is necessary.

The main reason I chose Purescript as the statically typed front-end was that:

* Purescript has a well-maintained and documented compiler-as-a-library
* Purescript's abstract syntax tree maps onto Python reasonably well (it's not
  perfect, but it's okay)

This library is architecturally very simple:

* Step 1 - Use the `purescript` library to parse Purescript code into an AST
* Step 2 - Write a pure function from a Purescript AST to a Python AST
* Step 3 - Use the `language-python` library to render the Python AST as source
  code

The goal is to strike a balance between making the Python code as idiomatic as
possible while still keeping the implementation reasonably simple.  Also, while
I'd like to support as much of Purescript as possible, I'm okay with not
supporting some language constructs if they prove too hairy to translate to
traditional Python idioms.

I'm not yet fully committed to this project.  I only put this up because some
people asked to see the code I had so far.  I will modify this README to
indicate when I'm taking this project more seriously.

In the meantime, if you would like to contribute, I will be very liberal about
accepting pull requests.

Right now the project is just a skeleton `Compile.hs` file showing the rough
outline of how things would work along with some example AST translations.  This
project is so unpolished that it's not even a `cabal` project yet.

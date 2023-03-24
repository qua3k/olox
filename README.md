# olox

OCaml implementation of the lox language from
[Crafting Interpreters](https://craftinginterpreters.com) (I have never used
OCaml before, but I have done some work with Haskell)

Use Jane Street's [style guide](https://opensource.janestreet.com/standards/).

## TODOs

*   Think about replacing the stdlib with Jane Street's Core.
    *   *Why this needs to be thought out:* I aim for this to have as few
        dependencies as possible, relying on easily understood OCaml code for
        beginners like me who are new to the language (avoiding ppx, etc)
    *   This also means that the code is easily to understand and avoids
        duplication, which I feel it does. The lexer is roughly 150 lines of
        code, and the parser and interpreter are roughly the same.
*   Figure out a way to return lexing errors and exiting before running the
    parser.
*   Profile the code

## Lessons Learned

*   Files implicitly define a module of their filename.
*   `List`s are singly-linked, rather than a contiguous array in memory that is
    reallocated when full a la Rust's `Vec<T>`. I presume this is due to the
    garbage collector, where it is easier to allocate a smaller object in the
    nursery and point the tail node to that rather than allocating the same
    list again. *Is this slow?*
*   Jane Street's replacement standard libraries (Base, Core) avoid raising
    exceptions unless they're postfixed with `_exn`.
*   OCaml will automatically run any expression of type `unit`, so functions
    called for the purpose of expressing side effects should be of prototype
    `unit -> unit`.
*   There is no `>>=` operator in the standard library, and the `let*` syntax
    described in
    [Language extensions](https://v2.ocaml.org/manual/bindingops.html) isn't
    actually defined either?
    *   Ah, it's just saying that forms of `let*` bindings were allowed, not
        that they're defined.
*   I have spent way too much time refactoring code that I didn't like, instead
    of actually writing more code.

## Interpreter Things

*   When synchronizing we want to parse the whole thing, but not interpret it

## Links

*   https://craftinginterpreters.com/scanning.html
*   https://keleshev.com/composable-error-handling-in-ocaml
*   https://cs.brown.edu/courses/cs017/content/docs/ocaml-style.pdf
*   https://github.com/graydon/rust-prehistory/blob/08ba0a74a7dd74b42c967b9c0263eb63627e9bb9/src/boot/fe/lexer.mll
*   https://www.cs.princeton.edu/courses/archive/fall13/cos326/lec/16b-laziness.pdf


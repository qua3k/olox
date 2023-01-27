# olox

OCaml implementation of the lox language from
[Crafting Interpreters](https://craftinginterpreters.com) (I have never used
OCaml before, but I have done some work with Haskell)

## Lessons Learned

*   Files implicitly define a module of their filename.
*   `List`s are singly-linked, rather than a contiguous array in memory that is
    reallocated when full a la Rust's `Vec<T>`. I presume this is due to the
    garbage collector, where it is easier to allocate a smaller object in the
    nursery and point the tail node to that rather than allocating the same
    list again.
*   Jane Street's replacement standard libraries (Base, Core) avoid raising
    exceptions.
*   OCaml will automatically run any expression of type `unit`, so functions
    called for the purpose of expressing side effects should be of prototype
    `unit -> unit`.
*   There is no `>>=` operator in the standard library, and the `let*` syntax
    described in
    [Language extensions](https://v2.ocaml.org/manual/bindingops.html) doesn't
    isn't actually defined either?


# Implementing Lox in Rust

Based on [Crafting Interpreters](https://craftinginterpreters.com/representing-code.html).

Implement a recursive-descent parser for the made up "Lox" language (made up for
the book).

## Current Status

Can parse the expression grammar specified below:

## expression grammar standalone

```text

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )*;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;

```

## Notes on the implementation

### Miette

I use the miette crate for pretty-printing errors, since it has specific
support for fancy output for text parsing e.g.

```bash

  ╭─[3:2]
 2 │ \notok
 3 │ "also not ok
   ·  ▲
   ·  ╰── here
   ╰────
```

### Strum

I use the strum crate for making it easier to display enums. This is partly
due to the funky requirements of CodeCrafters but also the book, since CC is
just enforcing the book's output.

### Thiserror, log and env-logger

I use the thiserror crate combined with the log and env-logger crates for nice
log messages rather than dbg! or e/println!

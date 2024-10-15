# Implementing Lox in Rust

Based on [Crafting Interpreters](https://craftinginterpreters.com/representing-code.html).

## expression grammar standalone

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )*;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;

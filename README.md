# Mylang

A very barebones programming language (lexer, parser, interpreter).

Has: Arithmetic, loops, conditionals, functions, a single integer type (unsigned 64-bit).

Has no: I/O, debugging facilities, compiler error messages (`Syntax error.` is all you get). Anything else, really.

## Here be dragons

The code is written for a one-off university project.
An old (pre 2018 edition) version of Rust is used,
the code is not very Rusty and not very nice in general.

One test is failing for some reason.
Can't be arsed to deal with that.

## Usage

```
$ cargo run example.mylang
```

This prints the generated VM instructions
and then runs the program, printing out the result of `main()`.
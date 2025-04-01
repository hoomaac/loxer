# Loxer

A Rust implementation of the Lox programming language interpreter, following the book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.

## About

This project is an interpreter for the Lox programming language, implemented in Rust. The implementation follows the concepts and techniques described in the "Crafting Interpreters" book, but adapts them to take advantage of Rust's features and idioms.

## Project Status

Currently implementing:
- [x] Lexical Analysis (Scanner/Tokenizer)
- [ ] Parser
- [ ] Abstract Syntax Tree
- [ ] Interpreter

## Usage

To tokenize a Lox source file:

```bash
cargo run -- tokenize path/to/your/source.lox
```

## License

[MIT License](LICENSE)
# SIMP-Interpreter
Interpreter for the simple imperative language (SIMP) written in Haskell.

## Description
SIMP is an imperative language created as an example to explore concepts such as operational semantics and abstract machines. I was very interested in both my programming languages course and my declarative programming course, implementing an abstract machine to learn more about operational semantics, and a parsing library to learn about functional design patterns like Monads. I decided to combine them in this project since interpreters have always fascinated me.

The parser is a top-down recursive descent parser. The library created for this parser is very similar to `parsec`.

To evaluate the syntax tree, a simple transition system was implemented using the operational semantics of the SIMP programming language.

## Running the Interpreter
To compile the program, simply run the command `make`.
To run the interpreter, execute the program with the file of the desired program, e.g., `./interpreter tests/prime.simp`.

## What I Learned
- Functional design patterns like Monads, Functors, and Applicatives.
- Top-down recursive descent parsing, using common functional parsing techniques.
- Writing a simple program evaluator using the operational semantics of a language.
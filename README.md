# Overview

This is a project for my undergrad Programming Languages class, not intended to be taken as an example of how to write a parser (or even idiomatic Racket).

> **NOTE**
> 
> My submission for this project is exclusively what lives in the parent folder:
> - main.rkt
> - parser.rkt
> - utility.rkt
> - test-parser.rkt 
> 
> I kept a series of sketches and ideas in the `sketches` folder because there were a ton of interesting ways to make this project happen and let's be honest, I worked too hard on some of those to just drop them completely. Some of them work, some of them don't, they're just saved for my own sake.

# Technical Details

This project relies on [Megaparsack](https://docs.racket-lang.org/megaparsack/), a parser combinator library. The decision to use parser combinators and Megaparsack in particular comes down to three things:

- ease of implementation
- readability
- expertise of the author


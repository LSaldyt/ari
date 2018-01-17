# Ari

Ari is a rewrite of [glossa](https://github.com/LSaldyt/glossa) written in Clojure.
Ari has several goals: 

- The parsing of any language specified in Backus-Naur form into a recursive dictionary structure.
- Then, a backend will be created that converts certain AST features into a low-level language.
- Then, the Ari software will be used to do research in language translation.
- Specifically, Ari will be used to research things like extensions to existing logic languages, simple semantic translation in a microdomain, and creation of more fluid programming languages.

These are just some basic ideas about what to do with ari.
First, though, I'll be building ari up to the point that glossa left off.
I found that C++, while efficient, was not semantically capable of handling abstract translations.
The existing Clojure solution uses about 10x less code, if not less.


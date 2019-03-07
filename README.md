# cl-hdl
yet another toy HDL DSL in Common Lisp

## overview

This is my attempt to write a little Common Lisp DSL (or S-expression syntax, we'll see how it turns out) for Verilog. I started this project on a whim - don't expect anything more than the last actual commit!

A few months ago, I learned about the [nanopass method of compiler construction](https://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf), and so I'm going to attempt to implement this project as a series of nanopasses. We'll see how far I get...

## status

Inactive for now, although I know what I need to do next: rewrite from a single-pass, emit-on-read strategy (where generated Verilog is emitted as it in generated) to something more like a real compiler, where you first make a pass and build up the AST for the entire program, and *then* have a separate code generation phase. That'll make the program much simpler in multiple ways.

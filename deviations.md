# Deviations from the book

Given the differences between Rust and OCaml the code and the book diverage, sometimes significantly.

In many cases the motivation for divergence is, I expect, reasonably clear.
That is, even if the Rust code could have been written otherwise, adhering to the OCaml was either impossible or unreasonable.

For example, many OCaml types are structs with fields, and corresponding OCaml functions implemented as methods on those structs.
Elsewhere, traits are defined to approximate OCaml's inference.
And, in other places borrows, clones, and lifetimes complicate things a little.

For those times where reason for divergence isn't so clear, or I thought a note seemed helpful, there's this document.


## General

Formulas are distinguished by structure, and then by operator or quantifier (where relevant) rather than individually, as in the book.
For example, a conjunction is a binary formula with an 'and' operator.

This helps structure matching in some cases.

## Propositional

## First order

### Scope

Quantifiers always take narrow scope, while in the book quantifiers always take wide scope.

### Substitutions

The book updates substitution functions by expands or retracts a given substitution function.

An analogue in Rust may be to expnd or retract something which implements the trait of a substitution function.
However, closures have unique types, and so this approach limits traits on functions which apply / manipulate substitutions.

The code adopts an ad-hoc solution of storing an 'interrupt' map with a general substitution function which supports interrupting specific substitution instances.
This works okay, so long as the updates themselves are ad-hoc (and they mostly seem to be).

### Variants

The book marks variants with `'`'s.
The Rust code uses a `variant` field on `Var`s and `Fun`s of type `usize` to track the variant.

Variants are postfixed to a `Var` / `Fun` with a distinguishing underscore.
So, for example, the 23rd variant of "v" would be seen as `v_23`.

A trailing underscore and sequence of digits is interpreted as variant information when creating `Var`s / `Fun`s from strings.

The change is made to make generating variants easier.

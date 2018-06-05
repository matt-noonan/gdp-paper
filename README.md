# Functional Pearl: Ghosts of Departed Proofs

## Abstract

Library authors often are faced with a design choice: should a function with preconditions be implemented as a partial function, or by returning a failure condition on incorrect use? Neither option is ideal. Partial functions lead to frustrating run-time errors. Failure conditions must be checked at the use-site, placing an unfair tax on the users who have ensured that the function’s preconditions were correctly met.

In this paper, we introduce an API design concept called “ghosts of departed proofs”. The key idea is that sophisticated preconditions can be encoded in Haskell’s type system with no run-time overhead, by using proofs that inhabit phantom type parameters attached to newtype wrappers. The user expresses correctness arguments by constructing proofs to inhabit these phantom type parameters. Critically, this technique allows the library user to decide when and how to validate that the APIs preconditions have been met.

The “ghosts of departed proofs” approach to API design can acheive many of the bene ts of dependent types and re nement types, while only requiring well-understood extensions to Haskell 2010. We demonstrate the utility of this approach through a series of case studies, showing how to enforce novel invariants for lists, maps, graphs, shared memory regions, and more.

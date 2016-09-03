An implementation of an evaluator for the lambda calculus, along with typechecking for any finite pure type system. A [pure type system](https://en.wikipedia.org/wiki/Pure_type_system) consists of a set of sorts, a set of axioms, and a set of relations.

By specifying these three sets appropriately, we can use all three axes of the lambda cube (dependent typing, quantification of types, type constructors). Thus this program can typecheck, e.g. the Simply Typed LC, System F, System F-Omega, and the Calculus of Constructions.

We can also specify inconsistent type systems, such as the one with one sort &lowast; with &lowast;:&lowast;.

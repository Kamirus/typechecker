# typechecker

## Project Idea

The main purpose of this project is to deeply understand the underlying type checking algorithm that is used in languages like PureScript.

The intention is to provide a well-documented implementation of a bidirectional type checker for a type system similar to PureScript's.

Thus the type system should extend the classical Hindley–Milner type system with higher rank polymorphism, type classes and possibly row polymorphism and kind polymorphism.

The implementation should combine the findings from several relevant papers that describe the type checking process for each needed feature of the type system.

My work begins with an implementation of the algorithm from the "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism".

Future work consists of adding recursive definitions and ADT before dealing with type classes which should be handled in a multi-parameter variant with functional dependencies.

Possible further extensions would include row polymorphism and kind polymorphism.


## Relevant Papers
- ["Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism"](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)
- ["Typing Haskell in Haskell"](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf)
- ["Type Classes with Functional Dependencies"](https://web.cecs.pdx.edu/~mpj/pubs/fundeps-esop2000.pdf)
- ["Kind Inference for Datatypes"](https://richarde.dev/papers/2020/kind-inference/kind-inference.pdf)
- ["Extensible records with scoped labels"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf)


# typechecker

## Project Idea

The main purpose of this project is to deeply understand the underlying type checking algorithm that is used in languages like PureScript.

The intention is to provide a well-documented implementation of a bidirectional type checker for a type system similar to PureScript's.

Thus the type system should extend the classical Hindley–Milner type system with higher rank polymorphism, type classes and possibly row polymorphism and kind polymorphism.

The implementation should combine the findings from several relevant papers that describe the type checking process for each needed feature of the type system.

My work begins with an implementation of the algorithm from the ["Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism"](https://www.cl.cam.ac.uk/~nk480/bidir.pdf).
Each jugdement from the *Algorithmic Type System* section needs to be implemented.

Next step would be to test the solution and provide some example terms together with their inferred types.

From there I aim to extend the term language with recursive let bindings and algebraic data types.
ADTs are essential to type class desugaring (Not necessary as I could express ADTs using Church encoding).
More research is required on how to integrate these features to the algorithmic type system from the paper.
I shall look for hints in the ["Typing Haskell in Haskell"](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf) or classic ["Putting Type Annotations to Work"](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.2659&rep=rep1&type=pdf)

Finally multi-parameter type classes with functional dependencies should be introduced.
The implementation should be based on the ["Type Classes with Functional Dependencies"](https://web.cecs.pdx.edu/~mpj/pubs/fundeps-esop2000.pdf).
<!-- Future work consists of adding recursive definitions and ADT before dealing with type classes which should be handled in a multi-parameter variant with functional dependencies. -->

Possible further extensions would include row polymorphism and kind polymorphism.

## Relevant Papers
- ["Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism"](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)
- ["Typing Haskell in Haskell"](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf)
- ["Putting Type Annotations to Work"](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.2659&rep=rep1&type=pdf)
- ["Type Classes with Functional Dependencies"](https://web.cecs.pdx.edu/~mpj/pubs/fundeps-esop2000.pdf)
- ["Kind Inference for Datatypes"](https://richarde.dev/papers/2020/kind-inference/kind-inference.pdf)
- ["Extensible records with scoped labels"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf)


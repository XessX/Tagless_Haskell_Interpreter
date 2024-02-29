# Tagless_Haskell_Interpreter
A tagless haskell interpreter built for 761-Generative Programming course(Jacques Carette) by me @XessX


An interpreters for a language with the following features:

Booleans, if-then-else\
pairs and projections\
lambda, application\
integers, addition, multiplication, (unary) minus\
ordering (i.e. less than) on integers\
equality on integers\
a combinator for computing fixed points 'fix'\
The interpreters to build (in finally tagless style) using Haskell:\

Following paper\
Typed Tagless Final Interpreters (https://okmij.org/ftp/tagless-final/course/lecture.pdf) \
Finally Tagless, Partially Evaluated (https://www.cas.mcmaster.ca/~carette/publications/jfp.pdf) \
Tutorial on Online Partial Evaluation (https://www.cs.utexas.edu/~wcook/tutorial/PEnotes.pdf) \
Multi-stage programming with functors and monads: eliminating abstraction overhead from generic code (https://www.cas.mcmaster.ca/~carette/publications/scp_metamonads.pdf) 

Following implementations and projects\
https://okmij.org/ftp/tagless-final/index.html \
https://www.tonicebrian.com/posts/2020/12/15/dominion.html \
https://hackmd.io/@gridtools/BJ-tiaCSY \
https://github.com/hermannhueck/tagless-final \
https://jameshfisher.com/2018/03/15/a-lambda-calculus-interpreter-in-haskell/ \
https://serokell.io/blog/introduction-tagless-final \
https://www.youtube.com/watch?v=_XoI65Rxmss&t=5s \
https://www.schoolofhaskell.com/user/mutjida/typed-tagless-final-linear-lambda-calculus/2-hoas-typed-tagless-final-interpreter \
https://github.com/blargoner/lambda 

As a usual programming language (i.e. that 'runs')\
that computes the length of the program\
that computes (using `Data.Text`) a valid Haskell representation of the program\
that computes (using `Data.Text`) a "pretty-printed" version of the program\
The code should come with a (commented!) test suite of programs that test all the features and all the interpreters. A standard unit test framework (such as HUnit or HTF) for that purpose is used.\

The code will be organized using either Stack or Cabal for doing builds. A README.md file is provided for documenting how to build the project and where to find the right pieces.\

The interpreter will likely have Partial Evaluation added to it as so called "Fancy Interpreter".


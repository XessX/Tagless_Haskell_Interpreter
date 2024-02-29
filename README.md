# Tagless_Haskell_Interpreter
A tagless haskell interpreter built for 761-Generative Programming course by me @XessX


An interpreters for a language with the following features:

booleans, if-then-else
pairs and projections
lambda, application
integers, addition, multiplication, (unary) minus
ordering (i.e. less than) on integers
equality on integers
a combinator for computing fixed points 'fix'
The interpreters to build (in finally tagless style) using Haskell:

Following paper\
Typed Tagless Final Interpreters (https://okmij.org/ftp/tagless-final/course/lecture.pdf)\
Finally Tagless, Partially Evaluated (https://www.cas.mcmaster.ca/~carette/publications/jfp.pdf)\
Tutorial on Online Partial Evaluation (https://www.cs.utexas.edu/~wcook/tutorial/PEnotes.pdf)\

as a usual programming language (i.e. that 'runs')
that computes the length of the program
that computes (using `Data.Text`) a valid Haskell representation of the program
that computes (using `Data.Text`) a "pretty-printed" version of the program
The code should come with a (commented!) test suite of programs that test all the features and all the interpreters. A standard unit test framework (such as HUnit or HTF) for that purpose is used.

The code is organized using either Stack or Cabal for doing builds. A README.md file is provided for documenting how to build the project and where to find the right pieces.


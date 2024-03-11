# Tagless_Haskell_Interpreter
A typed tagless haskell interpreter built for 761-Generative Programming course(Jacques Carette) by me @XessX

To run > dist-style > build > x(file_name) > haskell_project > haskell project(executable file) .
It will ask for permission to run and might give an error after running for the first time depending on your device but next time it'll run.

An interpreter with the following features:

Booleans, if-then-else\
pairs and projections\
lambda, application\
integers, addition, multiplication, (unary) minus\
ordering (i.e. less than) on integers\
equality on integers\
a combinator for computing fixed points 'fix'\
The interpreters to build (in finally tagless style) using Haskell:

As a usual programming language (i.e. that 'runs')\
that computes the length of the program\
that computes (using `Data.Text`) a valid Haskell representation of the program\
that computes (using `Data.Text`) a "pretty-printed" version of the program\
The code should come with a (commented!) test suite of programs that test all the features and all the interpreters. A standard unit test framework (such as HUnit or HTF) for that purpose is used.\

Haskell Ecosystem\
(https://github.com/Gabriella439/post-rfc/blob/main/sotu.md)

Haskell Tagless Cookbook\
https://okmij.org/ftp/tagless-final/cookbook.html

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
https://jameshfisher.com/2018/03/15/a-lambda-calculus-interpreter-in-haskell \
https://serokell.io/blog/introduction-tagless-final \
https://www.youtube.com/watch?v=_XoI65Rxmss&t=5s \
https://www.schoolofhaskell.com/user/mutjida/typed-tagless-final-linear-lambda-calculus/2-hoas-typed-tagless-final-interpreter \
https://github.com/blargoner/lambda 

As a usual programming language (i.e. that 'runs')\
that computes the length of the program\
that computes (using `Data.Text`) a valid Haskell representation of the program\
that computes (using `Data.Text`) a "pretty-printed" version of the program\
The code should come with a (commented!) test suite of programs that test all the features and all the interpreters. A standard unit test framework (such as HUnit or HTF) for that purpose is used.<br /> 

For some expressive ideas and codebase which provided me some understanding of the pipeline, was this source - (https://github.com/bren007pie/CAS761/blob/main/Assignment%203/GiveOut.hs), thanks to Bren.<br />

Codes/Materials for understanding the project | Followed studies to build the Project
------------- | -------------
Data Types & Class Types  | (https://okmij.org/ftp/tagless-final/course/lecture.pdf) - [3.4 - Tagless Final embedding]<br /> (https://www.cas.mcmaster.ca/~carette/publications/jfp.pdf) - [2.1 - How to make encoding flexible: abstract the interpreter]<br />(https://okmij.org/ftp/tagless-final/index.html#course-oxford)<br />(https://serokell.io/blog/introduction-tagless-final)
Length of the Program  |  (https://gist.github.com/animatedlew/8138942)<br />(https://www.cas.mcmaster.ca/~carette/publications/jfp.pdf) - [2.2 Two tagless interpreters, proposition 3]<br />I was not fully sure about computing length of the program, as after I did ExpSym Runs , "As a usual programming language (i.e. that 'runs')" , I read through paper (i.e. JFP) and skim through some online sites which helped me to understand.<br /> Name change - [page-30, Oleg's Tagless Final "Since R (which is the name for the interpreter, not a type tag) is a newtype, at run-time, R x is indistinguishable from x. It becomes obvious that the interpreter R is meta-circular: object-language integers are the Haskell integers themselves; object-language addition is Haskell addition and object-language application is Haskell application."]<br /> 
Program that Runs  | (https://okmij.org/ftp/tagless-final/course/lecture.pdf) - [3.4 Tagless final embedding, 29page, 27 page]
Data.Text for Haskell & Pretty Print Representation | (https://hackage.haskell.org/package/text-2.1.1/docs/Data-Text.html) - Data.Text(Text) , Data.Text.IO (Outputs Hx, Pretty print when type is Text) , T.Pack (String -> Text), T.concat (concatenates Text)<br />(https://okmij.org/ftp/tagless-final/course/lecture.pdf)-[page-28 Pretty-print & Transformer]<br />(https://tarmean.github.io/prettyprinter.html)<br />(https://stackoverflow.com/questions/58960669/using-annotations-and-pretty-printer)<br /> (https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html#g:5)<br />(https://github.com/quchen/show-prettyprint/blob/master/src/Text/Show/Prettyprint.hs)<br /> [This/prettyprinter was quite hard to understand at first yet it will take more time to understand this more or less]
TestSuite  | (https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/Test-HUnit.html)
HUnit TestCases  |  (https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/Test-HUnit.html)<br />(https://courses.cs.washington.edu/courses/cse341/18sp/haskell/running.html)
Cabal Build  | (https://www.haskell.org/ghcup/steps/)<br />When installing dependencies, don't forget to use 'cabal install text --lib', same goes for HUnit and PrettyPrinter
Fix Combinator (Extra) | (https://okmij.org/ftp/Haskell/types.html#Prepose)
Misc | (https://typeclasses.substack.com/p/whats-new-in-ghc-2021)<br />(https://www.schoolofhaskell.com/user/mutjida/typed-tagless-final-linear-lambda-calculus/2-hoas-typed-tagless-final-interpreter)<br />(https://maksbotan.github.io/posts/2019-09-04-abstract-definitional-interpreters.html)<br />   (https://www.reddit.com/r/haskell/comments/424txm/what_is_the_advantage_of_tagless_final_style_over)<br />(https://www.youtube.com/watch?v=_KioQRICpmo)<br />   (https://discuss.ocaml.org/t/explain-like-im-5-years-old-tagless-final-pattern/9394/13)


The interpreter will likely have Partial Evaluation added to it as so called "Fancy Interpreter".

I thank everyone who helped me to learn more about Haskell Programming.


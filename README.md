# fraktell
Fractal visualization in haskell

# running:
Run the following in the fraktell directory:

    stack build
    stack exec fraktell-exe

or, alternatively:

    stack run

examples:

    stack run 1.5 1000 1000 100 "images/output.png"
    stack run 1.5 1000 1000 100 "images/output.png" RPU
    stack run 1.5 1000 1000 100 "images/output.png" VU 1 "(0.285 :+ 0)"
    stack run -- --output benchmark.html

in order to use multiple cores, be sure to specify the number of cores with the
flag `+RTS -Nx`, where x is the number of cores. For example:

    stack run +RTS -N16 -RTS 1.5 9000 9000 100 "images/output.png"

`+RTS` means the following arguments should go to the haskell RunTime System.
`-RTS` means the following argumemts should go to the program itself. This is
how we can have both program arguments and meta-arguments for the haskell
runtime

the final example, `stack run b` will benchmark the program.

## tests
    stack test

# parameters I've discovered that generate interesting parameters:
all for function f(z) = x^2 + c

first thing I generated used c = -0.4 + 0.65i

the scripts "fraktell-generations.py" and "interesting-fractals.sh" are good
demos

# finding performance:
a performance-intensive computation is with f = z^2 + c where c = -0.61803 + 0i
and max iterations is 1000 (this will frequently reach max iterations)

# goals / language accomplishments
 * demonstrate good software development practices
   * unittests
 * leverage functional nature of haskell
   * mathematical functions for which we will create julia sets
   * partial application of functions (see Spec.hs "should use partial
     application..." case)
 * leverage haskell's lazy evaluation
   * see runWithCliArgs: even if arrayType or func_num are not supplied, the
     array indexing (lines 46, 47) will not fail!
 * explore parallelism in haskell
   * show speedup when working with large images
   * benchmarking in haskell is difficult.
     * in c: "start timing", "end timing"
     * but in functional languages, the order in which you write things is not
       necessarily the order things are executed in (in fact, it's often not
       even close to the order of execution)
     * there are some good frameworks, like criterion
     * even these frameworks are hard
   * ended up timing entire execution:
     * this isn't a GREAT test because saving to disk takes the most time and
       is sequential, but it's better than nothing
 * write as much as possible in haskell: don't rely too heavily on external
   scripts and things

# next steps
 * write speedtest
 * benchmark on mamba
 * put benchmarks in `benchmark` folder and commit
 * visualize speedup
 * generate large images
 * work on speedup
 * play around with different fractals that we can generate

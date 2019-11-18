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
    stack run 1.5 1000 1000 100 "images/output.png" 1 "(0.285 :+ 0)"
    stack run 1.5 1000 1000 100 "images/output.png" RPU
    stack run -- --output benchmark.html

the final example, `stack run b` will benchmark the program.

## tests
    stack test

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

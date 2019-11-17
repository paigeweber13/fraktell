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

## tests
    stack test

# goals
 * demonstrate good software development practices
   * unittests
 * leverage functional nature of haskell
   * mathematical functions for which we will create julia sets
   * partial application of functions (see Spec.hs "should use partial
     application..." case)
 * explore parallelism in haskell
   * show speedup when working with large images

# next steps
 * write speedtest
 * generate large images
 * work on speedup
 * play around with different fractals that we can generate

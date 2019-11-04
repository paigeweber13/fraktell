# fraktell
Fractal visualization in haskell

# running:
Run the following in the fraktell directory:

    stack build
    stack exec fraktell-exe

or, alternatively:

    stack run

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

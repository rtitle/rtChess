cabal clean && cabal configure --enable-tests && cabal build
runhaskell -isrc -itest test/Spec.hs

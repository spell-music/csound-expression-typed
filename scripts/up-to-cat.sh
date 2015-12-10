#!usr/bin/sh
cabal install --force-reinstalls
cd ../csound-expression
cabal install --force-reinstalls
cd ../csound-sampler
cabal install --force-reinstalls
cd ../csound-catalog
cabal install --force-reinstalls

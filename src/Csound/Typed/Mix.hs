module Csound.Typed.Mix where

import Csound.Dynamic
import Csound.Typed.Tuple
import Csound.Typed.Arg

data Mix a = Mix

sco :: (CsdSco f, Arg a, Out b) => f a -> (a -> b) -> f (Mix b)
sco = undefined

mix :: (CsdSco f, Out a, Out b) => (a -> b) -> f (Mix a) -> f (Mix b)
mix = undefined

runMix :: (CsdSco f, Out a) => f (Mix a) -> NoSE a
runMix = undefined


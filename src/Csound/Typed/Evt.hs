module Csound.Typed.Evt where

import Csound.Typed.Types
import Csound.Typed.Tuple
import Csound.Typed.Arg

data Evt a = Evt

sched :: (Arg a, Out b) => Evt (D, a) -> (a -> b) -> NoSE b
sched = undefined

schedHarp :: (Arg a, Out b) => Evt a -> (a -> b) -> NoSE b
schedHarp = undefined


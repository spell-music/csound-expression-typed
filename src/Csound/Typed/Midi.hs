module Csound.Typed.Midi where

import Csound.Typed.Tuple
import Csound.Typed.Evt

data Msg = Msg

type Channel = Int

midi :: Out a => Channel -> (Msg -> a) -> NoSE a
midi = undefined

midiE :: Channel -> Evt Msg
midiE = undefined

{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Midi(midiE, midi, pgmidi) where

import Csound.Typed.Types
import Csound.Typed.Types.GlobalState
import Csound.Typed.Types.GlobalState.Instr
import Csound.Typed.Control.Evt

midiE :: Channel -> Evt Msg
midiE = undefined

-- | Triggers a midi-instrument (aka Csound's massign). 
midi :: (Out a, Out (NoSE a)) => Channel -> (Msg -> a) -> NoSE a
midi = genMidi Massign

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi :: (Out a, Out (NoSE a)) => Maybe Int -> Channel -> (Msg -> a) -> NoSE a
pgmidi mchn = genMidi (Pgmassign mchn)

genMidi :: (Out a, Out (NoSE a)) => MidiType -> Channel -> (Msg -> a) -> NoSE a
genMidi midiType chn instr = fromOut $ do
    setDurationToInfinite
    saveMidiInstr midiType chn instr


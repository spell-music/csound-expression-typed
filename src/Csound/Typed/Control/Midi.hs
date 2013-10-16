{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Midi(
    midiE, midi, pgmidi, Msg, Channel
) where

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

midiE :: Channel -> Evt Msg
midiE = undefined

-- | Triggers a midi-instrument (aka Csound's massign). 
midi :: (Sigs a) => Channel -> (Msg -> SE a) -> a
midi = genMidi Massign

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi :: (Sigs a) => Maybe Int -> Channel -> (Msg -> SE a) -> a
pgmidi mchn = genMidi (Pgmassign mchn)

genMidi :: (Sigs a) => MidiType -> Channel -> (Msg -> SE a) -> a
genMidi midiType chn instr = toTuple $ setDurationToInfinite >>
    saveMidiInstr midiType chn (constArity $ instr Msg) (midiExp instr)


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
midi :: (Out a, Out (NoSE a)) => Channel -> (Msg -> a) -> NoSE a
midi = genMidi Massign

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi :: (Out a, Out (NoSE a)) => Maybe Int -> Channel -> (Msg -> a) -> NoSE a
pgmidi mchn = genMidi (Pgmassign mchn)

genMidi :: (Out a, Out (NoSE a)) => MidiType -> Channel -> (Msg -> a) -> NoSE a
genMidi midiType chn instr = fromOut $ do
    setDurationToInfinite
    sigs <- saveMidiInstr midiType chn (midiArity instr) (midiExp instr)
    return $ fmap fromE sigs


{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Midi(
    Msg, Channel,
    midiE, 
    midi, midin, pgmidi, 
    midi_, midin_, pgmidi_
) where

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

midiE :: Channel -> Evt Msg
midiE = undefined

midi :: (Sigs a) => (Msg -> SE a) -> a
midi = midin 0

-- | Triggers a midi-instrument (aka Csound's massign). 
midin :: (Sigs a) => Channel -> (Msg -> SE a) -> a
midin = genMidi Massign

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi :: (Sigs a) => Maybe Int -> Channel -> (Msg -> SE a) -> a
pgmidi mchn = genMidi (Pgmassign mchn)

genMidi :: (Sigs a) => MidiType -> Channel -> (Msg -> SE a) -> a
genMidi midiType chn instr = toTuple $ setDurationToInfinite >>
    saveMidiInstr midiType chn (constArity $ instr Msg) (midiExp instr)

-----------------------------------------------------------------
--

midi_ :: (Msg -> SE ()) -> SE ()
midi_ = midin_ 0

-- | Triggers a midi-instrument (aka Csound's massign). 
midin_ :: Channel -> (Msg -> SE ()) -> SE ()
midin_ = genMidi_ Massign

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi_ :: Maybe Int -> Channel -> (Msg -> SE ()) -> SE ()
pgmidi_ mchn = genMidi_ (Pgmassign mchn)

genMidi_ :: MidiType -> Channel -> (Msg -> SE ()) -> SE ()
genMidi_ midiType chn instr = fromDep_ $ setDurationToInfinite >>
    saveMidiInstr_ midiType chn (unitExp $ instr' Msg)
    where instr' = unit . instr

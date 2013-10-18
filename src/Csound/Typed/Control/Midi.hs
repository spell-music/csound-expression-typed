{-# Language TypeFamilies, FlexibleContexts #-}
module Csound.Typed.Control.Midi(
    Msg, Channel,
    midiE, 
    midi, midin, pgmidi, 
    midi', midin', pgmidi',
    midi_, midin_, pgmidi_
) where

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

import Csound.Typed.Control.Overload

midiE :: Channel -> Evt Msg
midiE = undefined

midi :: (MidiInstr f, Sigs (MidiInstrOut f)) => f -> MidiInstrOut f
midi f = midi' (toMidiInstr f)

midi' :: (Sigs a) => (Msg -> SE a) -> a
midi' = midin' 0

-- | Triggers a midi-instrument (aka Csound's massign). 
midin :: (MidiInstr f, Sigs (MidiInstrOut f)) => Channel -> f -> MidiInstrOut f
midin n f = midin' n (toMidiInstr f)

midin' :: (Sigs a) => Channel -> (Msg -> SE a) -> a
midin' = genMidi Massign

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi :: (MidiInstr f, Sigs (MidiInstrOut f)) => Maybe Int -> Channel -> f -> MidiInstrOut f
pgmidi n m f = pgmidi' n m (toMidiInstr f)

pgmidi' :: (Sigs a) => Maybe Int -> Channel -> (Msg -> SE a) -> a
pgmidi' mchn = genMidi (Pgmassign mchn)

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

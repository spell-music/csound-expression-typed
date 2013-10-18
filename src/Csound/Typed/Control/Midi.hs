{-# Language TypeFamilies, FlexibleContexts #-}
module Csound.Typed.Control.Midi(
    Msg, Channel,
    midiE, 
    midi, midin, pgmidi, 
    midi', midin', pgmidi',
    midi_, midin_, pgmidi_
) where

import System.Mem.StableName

import Control.Applicative
import Control.Monad.IO.Class
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
midin n f = genMidi Massign n (toMidiInstr f) f

midin' :: (Sigs a) => Channel -> (Msg -> SE a) -> a
midin' n f = genMidi Massign n f f

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi :: (MidiInstr f, Sigs (MidiInstrOut f)) => Maybe Int -> Channel -> f -> MidiInstrOut f
pgmidi mchn n f = genMidi (Pgmassign mchn) n (toMidiInstr f) f

pgmidi' :: (Sigs a) => Maybe Int -> Channel -> (Msg -> SE a) -> a
pgmidi' mchn n f = genMidi (Pgmassign mchn) n f f

genMidi :: (Sigs a) => MidiType -> Channel -> (Msg -> SE a) -> b -> a
genMidi midiType chn instr cacheName = toTuple $ do    
    key <- midiKey midiType chn cacheName
    withCache getMidiKey saveMidiKey key $
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
genMidi_ midiType chn instr = fromDep_ $ do
    key <- midiKey midiType chn instr
    withCache getMidiProcKey saveMidiProcKey key $ 
        saveMidiInstr_ midiType chn (unitExp $ unit $ instr Msg)

-----------------------------------------------------------------

midiKey :: MidiType -> Channel -> a -> GE MidiKey
midiKey ty chn a = liftIO $ MidiKey ty chn . hashStableName <$> makeStableName a  


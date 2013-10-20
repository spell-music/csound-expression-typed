{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Midi(
    Msg, Channel,
    midi, midin, pgmidi, 
    midi_, midin_, pgmidi_
) where

import System.Mem.StableName

import Control.Applicative
import Control.Monad.IO.Class
import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

midi :: (Sigs a) => (Msg -> SE a) -> a
midi = midin 0

-- | Triggers a midi-instrument (aka Csound's massign). 
midin :: (Sigs a) => Channel -> (Msg -> SE a) -> a
midin n f = genMidi Massign n f

-- | Triggers a - midi-instrument (aka Csound's pgmassign). 
pgmidi :: (Sigs a) => Maybe Int -> Channel -> (Msg -> SE a) -> a
pgmidi mchn n f = genMidi (Pgmassign mchn) n f 

genMidi :: (Sigs a) => MidiType -> Channel -> (Msg -> SE a) -> a
genMidi midiType chn instr = toTuple $ do    
    key <- midiKey midiType chn instr
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


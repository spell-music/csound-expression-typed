{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Midi(
    Msg, Channel,
    midi, midin, pgmidi, 
    midi_, midin_, pgmidi_,
    initMidiCtrl
) where

import System.Mem.StableName

import Control.Applicative
import Control.Monad.IO.Class
import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

-- | Triggers a midi-instrument (aka Csound's massign) for all channels. 
-- It's useful to test a single instrument.
midi :: (Sigs a) => (Msg -> SE a) -> a
midi = midin 0

-- | Triggers a midi-instrument (aka Csound's massign) on the specified channel. 
midin :: (Sigs a) => Channel -> (Msg -> SE a) -> a
midin n f = genMidi Massign n f

-- | Triggers a midi-instrument (aka Csound's pgmassign) on the specified programm bank. 
pgmidi :: (Sigs a) => Maybe Int -> Channel -> (Msg -> SE a) -> a
pgmidi mchn n f = genMidi (Pgmassign mchn) n f 

genMidi :: (Sigs a) => MidiType -> Channel -> (Msg -> SE a) -> a
genMidi midiType chn instr = toTuple $ do    
    key <- midiKey midiType chn instr
    withCache InfiniteDur getMidiKey saveMidiKey key $
        saveMidiInstr midiType chn (constArity $ instr Msg) (midiExp instr)

-----------------------------------------------------------------
--

-- | Triggers a midi-procedure (aka Csound's massign) for all channels. 
midi_ :: (Msg -> SE ()) -> SE ()
midi_ = midin_ 0

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given channel. 
midin_ :: Channel -> (Msg -> SE ()) -> SE ()
midin_ = genMidi_ Massign

-- | Triggers a midi-procedure (aka Csound's pgmassign) on the given programm bank. 
pgmidi_ :: Maybe Int -> Channel -> (Msg -> SE ()) -> SE ()
pgmidi_ mchn = genMidi_ (Pgmassign mchn)

genMidi_ :: MidiType -> Channel -> (Msg -> SE ()) -> SE ()
genMidi_ midiType chn instr = fromDep_ $ hideGEinDep $ do
    key <- midiKey midiType chn instr
    withCache InfiniteDur getMidiProcKey saveMidiProcKey key $ 
        saveMidiInstr_ midiType chn (unitExp $ fmap (const unit) $ instr Msg)

-----------------------------------------------------------------

midiKey :: MidiType -> Channel -> a -> GE MidiKey
midiKey ty chn a = liftIO $ MidiKey ty chn . hashStableName <$> makeStableName a  

-----------------------------------------------------------------
-- midi ctrls

initMidiCtrl :: D -> D -> D -> SE ()
initMidiCtrl chno ctrlno val = geToSe $ 
    saveMidiCtrl =<< (MidiCtrl <$> toGE chno <*> toGE ctrlno <*> toGE val)


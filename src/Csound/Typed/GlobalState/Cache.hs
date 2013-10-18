module Csound.Typed.GlobalState.Cache(
    Cache(..), HashKey,
    saveMidiKey, getMidiKey,

    CacheMidi, MidiKey(..), Channel, MidiType(..)
) where

import qualified Data.Map as M
import Data.Default

import Csound.Dynamic

data Cache = Cache 
    { cacheMidi     :: CacheMidi }

instance Default Cache where
    def = Cache def

type HashKey = Int

getMidiKey :: MidiKey -> Cache -> Maybe [E]
getMidiKey key x = M.lookup key $ cacheMidi x

saveMidiKey :: MidiKey -> [E] -> Cache -> Cache
saveMidiKey key val x = x { cacheMidi = M.insert key val (cacheMidi x) } 

-- Midi

type Channel = Int

data MidiType = Massign | Pgmassign (Maybe Int)
    deriving (Eq, Ord)

data MidiKey = MidiKey MidiType Channel HashKey
    deriving (Eq, Ord)

type CacheMidi = M.Map MidiKey [E]


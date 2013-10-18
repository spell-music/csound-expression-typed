module Csound.Typed.GlobalState.Cache(
    Cache(..), HashKey,

    -- * Midi
    -- ** Functions
    CacheMidi, MidiKey(..), Channel, MidiType(..),
    saveMidiKey, getMidiKey,
    -- ** Procedures
    CacheMidiProc, 
    saveMidiProcKey, getMidiProcKey,
    -- * Mix
    -- ** Functions
    CacheMix, MixKey(..),
    saveMixKey, getMixKey,
    -- ** Procedures
    CacheMixProc, 
    saveMixProcKey, getMixProcKey,
    -- * Evt
    -- ** Functions
    CacheEvt, EvtKey(..),
    saveEvtKey, getEvtKey,
    -- ** Procedures
    CacheEvtProc, 
    saveEvtProcKey, getEvtProcKey
) where

import qualified Data.Map as M
import Data.Default

import Csound.Dynamic

data Cache = Cache 
    { cacheMidi     :: CacheMidi 
    , cacheMidiProc :: CacheMidiProc
    , cacheMix      :: CacheMix
    , cacheMixProc  :: CacheMixProc
    , cacheEvt      :: CacheEvt
    , cacheEvtProc  :: CacheEvtProc }

instance Default Cache where
    def = Cache def def def def def def

type HashKey = Int

type GetKey  a b = a -> Cache -> Maybe b
type SaveKey a b = a -> b -> Cache -> Cache

getKeyMap :: (Ord key) => (Cache -> M.Map key val) -> GetKey key val
getKeyMap f key x = M.lookup key $ f x

saveKeyMap :: (Ord key) => (Cache -> M.Map key val) -> (M.Map key val -> Cache -> Cache) -> SaveKey key val
saveKeyMap getter setter key val cache = setter (M.insert key val $ getter cache) cache

----------------------------------------------------------
-- Midi

type Channel = Int

data MidiType = Massign | Pgmassign (Maybe Int)
    deriving (Eq, Ord)

data MidiKey = MidiKey MidiType Channel HashKey
    deriving (Eq, Ord)

-- Midi functions

type CacheMidi = M.Map MidiKey [E]

getMidiKey :: GetKey MidiKey [E]
getMidiKey = getKeyMap cacheMidi

saveMidiKey :: SaveKey MidiKey [E]
saveMidiKey = saveKeyMap cacheMidi (\a x -> x { cacheMidi = a })

-- Midi procedures

type CacheMidiProc = M.Map MidiKey (Dep ())

getMidiProcKey :: GetKey MidiKey (Dep ())
getMidiProcKey = getKeyMap cacheMidiProc

saveMidiProcKey :: SaveKey MidiKey (Dep ())
saveMidiProcKey = saveKeyMap cacheMidiProc (\a x -> x { cacheMidiProc = a })

----------------------------------------------------------
-- Mix

-- Mix functions

newtype MixKey = MixKey HashKey
    deriving (Eq, Ord)

type    MixVal = InstrId

type CacheMix = M.Map MixKey MixVal

getMixKey :: GetKey MixKey MixVal
getMixKey = getKeyMap cacheMix

saveMixKey :: SaveKey MixKey MixVal
saveMixKey = saveKeyMap cacheMix (\a x -> x { cacheMix = a })

-- Mix procedures

type CacheMixProc = M.Map MixKey (Dep ())

getMixProcKey :: GetKey MixKey (Dep ())
getMixProcKey = getKeyMap cacheMixProc

saveMixProcKey :: SaveKey MixKey (Dep ())
saveMixProcKey = saveKeyMap cacheMixProc (\a x -> x { cacheMixProc = a })

----------------------------------------------------------
-- Evt

-- Evt functions

data EvtKey = EvtKey HashKey HashKey
    deriving (Eq, Ord)

type    EvtVal = InstrId

type CacheEvt = M.Map EvtKey EvtVal

getEvtKey :: GetKey EvtKey EvtVal
getEvtKey = getKeyMap cacheEvt

saveEvtKey :: SaveKey EvtKey EvtVal
saveEvtKey = saveKeyMap cacheEvt (\a x -> x { cacheEvt = a })

-- Evt procedures

type CacheEvtProc = M.Map EvtKey (Dep ())

getEvtProcKey :: GetKey EvtKey (Dep ())
getEvtProcKey = getKeyMap cacheEvtProc

saveEvtProcKey :: SaveKey EvtKey (Dep ())
saveEvtProcKey = saveKeyMap cacheEvtProc (\a x -> x { cacheEvtProc = a })


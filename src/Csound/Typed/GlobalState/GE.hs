module Csound.Typed.GlobalState.GE(
    GE, History(..), withOptions, getOptions, execGE,
    -- * Globals
    onGlobals, 
    -- * Midi
    MidiAssign(..), Msg(..), renderMidiAssign, saveMidi,  
    -- * Instruments
    setMasterInstrId, onInstr, saveUserInstr0, getSysExpr,
    -- * Total duration
    TotalDur(..), getTotalDur, setDuration, setDurationToInfinite,
    -- * GEN routines
    saveGen,
    -- * Band-limited waves
    saveBandLimitedWave,
    -- * Strings
    saveStr,
    -- * Cache
    -- ** Midi
    getMidiInstrFromCache, saveMidiInstrToCache
) where

import Control.Applicative
import Control.Monad
import Data.Default

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Csound.Dynamic 
import Csound.Dynamic.Control

import Csound.Typed.GlobalState.Options
import Csound.Typed.GlobalState.Cache

-- global side effects
newtype GE a = GE { unGE :: ReaderT Options (StateT History IO) a }

runGE :: GE a -> Options -> History -> IO (a, History)
runGE (GE f) opt hist = runStateT (runReaderT f opt) hist

execGE :: Options -> GE a -> IO History
execGE options a = fmap snd $ runGE a options def

instance Functor GE where
    fmap f = GE . fmap f . unGE

instance Applicative GE where
    pure = return
    (<*>) = ap

instance Monad GE where
    return = GE . return
    ma >>= mf = GE $ unGE ma >>= unGE . mf

instance MonadIO GE where
    liftIO = GE . liftIO . liftIO
    
data History = History
    { genMap            :: GenMap
    , stringMap         :: StringMap
    , globals           :: Globals
    , instrs            :: Instrs
    , midis             :: [MidiAssign]
    , totalDur          :: Maybe TotalDur
    , masterInstrId     :: InstrId
    , userInstr0        :: InstrBody
    , bandLimitedMap    :: BandLimitedMap
    , cache             :: Cache }

instance Default History where
    def = History def def def def def def (intInstrId 0) (return ()) def def

data Msg = Msg
data MidiAssign = MidiAssign MidiType Channel InstrId
            
renderMidiAssign :: MidiAssign -> Dep ()
renderMidiAssign (MidiAssign ty chn instrId) = case ty of
    Massign         -> massign chn instrId
    Pgmassign mn    -> pgmassign chn instrId mn
    where
        massign n instr = dep_ $ opcs "massign" [(Xr, [Ir,Ir])] [int n, prim $ PrimInstrId instr]
        pgmassign pgm instr mchn = dep_ $ opcs "pgmassign" [(Xr, [Ir,Ir,Ir])] ([int pgm, prim $ PrimInstrId instr] ++ maybe [] (return . int) mchn)

data TotalDur = NumDur Double | InfiniteDur
    deriving (Eq, Ord)

getTotalDur :: Options -> (Maybe TotalDur) -> Double
getTotalDur opt = toDouble . maybe InfiniteDur id  
    where 
        toDouble x = case x of
            NumDur d    -> d
            InfiniteDur -> setInfiniteDur opt

saveStr :: String -> GE E
saveStr = fmap prim . onStringMap . newString
    where onStringMap = onHistory stringMap (\val h -> h{ stringMap = val })

saveGen :: Gen -> GE E
saveGen = onGenMap . newGen
    where onGenMap = onHistory genMap (\val h -> h{ genMap = val })

saveBandLimitedWave :: BandLimited -> GE Int
saveBandLimitedWave = onBandLimitedMap . saveBandLimited
    where onBandLimitedMap = onHistory 
                (\a -> (genMap a, bandLimitedMap a)) 
                (\(gm, blm) h -> h { genMap = gm, bandLimitedMap = blm})

setDurationToInfinite :: GE ()
setDurationToInfinite = setTotalDur InfiniteDur

setDuration :: Double -> GE ()
setDuration = setTotalDur . NumDur

setTotalDur :: TotalDur -> GE ()
setTotalDur = onTotalDur . modify . max . Just
    where onTotalDur = onHistory totalDur (\a h -> h { totalDur = a })

saveMidi :: MidiAssign -> GE ()
saveMidi ma = onMidis $ modify (ma: )
    where onMidis = onHistory midis (\a h -> h { midis = a })

saveUserInstr0 :: InstrBody -> GE ()
saveUserInstr0 expr = onUserInstr0 $ modify ( >> expr)
    where onUserInstr0 = onHistory userInstr0 (\a h -> h { userInstr0 = a })

getSysExpr :: GE (Dep ())
getSysExpr = fmap sequence_ $ sequence [ onGlobals $ clearGlobals ]    
    where
        clearGlobals :: State Globals (Dep ())
        clearGlobals = gets $ mapM_ (flip writeVar 0) . globalsClearable 

setMasterInstrId :: InstrId -> GE ()
setMasterInstrId masterId = onMasterInstrId $ put masterId
    where onMasterInstrId = onHistory masterInstrId (\a h -> h { masterInstrId = a })

----------------------------------------------------------------------
-- state modifiers

withOptions :: (Options -> a) -> GE a
withOptions f = GE $ asks f

getOptions :: GE Options
getOptions = withOptions id

withHistory :: (History -> a) -> GE a
withHistory f = GE $ lift $ fmap f get

modifyHistory :: (History -> History) -> GE ()
modifyHistory = GE . lift . modify

-- update fields

onHistory :: (History -> a) -> (a -> History -> History) -> State a b -> GE b
onHistory getter setter st = GE $ ReaderT $ \_ -> StateT $ \history -> 
    let (res, s1) = runState st (getter history)
    in  return (res, setter s1 history) 

type UpdField a b = State a b -> GE b

onInstr :: UpdField Instrs a
onInstr = onHistory instrs (\a h -> h { instrs = a })

onGlobals :: UpdField Globals a
onGlobals = onHistory globals (\a h -> h { globals = a })

----------------------------------------------------------------------
-- cache

getMidiInstrFromCache :: MidiKey -> GE (Maybe [E])
getMidiInstrFromCache key = withHistory $ getMidiKey key . cache

saveMidiInstrToCache :: MidiKey -> [E] -> GE ()
saveMidiInstrToCache key val = modifyHistory $ \h -> h { cache = saveMidiKey key val (cache h) }


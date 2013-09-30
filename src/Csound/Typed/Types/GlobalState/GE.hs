module Csound.Typed.Types.GlobalState.GE where

import Control.Applicative
import Control.Monad
import Data.Default

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Csound.Dynamic 
import Csound.Dynamic.Control

import Csound.Typed.Types.GlobalState.Options

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
    { genMap        :: GenMap
    , stringMap     :: StringMap
    , globals       :: Globals
    , locals        :: Locals
    , instrs        :: Instrs
    , midis         :: [MidiAssign]
    , totalDur      :: Maybe TotalDur
    , masterInstrId :: InstrId
    , userInstr0    :: InstrBody }

type Channel = Int
data MidiType = Massign | Pgmassign (Maybe Int)
data Msg = Msg
data MidiAssign = MidiAssign MidiType Channel InstrId
            
renderMidiAssign :: MidiAssign -> Dep ()
renderMidiAssign (MidiAssign ty chn instrId) = case ty of
    Massign         -> massign chn instrId
    Pgmassign mn    -> pgmassign chn instrId mn
    where
        massign n instr = dep_ $ opcs "massign" [(Xr, [Ir,Ir])] [int n, prim $ PrimInstrId instr]
        pgmassign pgm instr mchn = dep_ $ opcs "pgmassign" [(Xr, [Ir,Ir,Ir])] ([int pgm, prim $ PrimInstrId instr] ++ maybe [] (return . int) mchn)

instance Default History where
    def = History def def def def def def def (intInstrId 0) (return ())

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

setDurationToInfinite :: GE ()
setDurationToInfinite = onTotalDur $ modify (max $ Just InfiniteDur)

setDuration :: Double -> GE ()
setDuration = onTotalDur . modify . max . Just . NumDur

saveMidi :: MidiAssign -> GE ()
saveMidi a = onMidis $ modify (a: )

saveUserInstr0 :: InstrBody -> GE ()
saveUserInstr0 expr = onUserInstr0 $ modify ( >> expr)

getSysExpr :: GE (Dep ())
getSysExpr = fmap sequence_ $ sequence [ onGlobals $ clearGlobals ]    
    where
        clearGlobals :: State Globals (Dep ())
        clearGlobals = gets $ mapM_ (flip writeVar 0) . globalsClearable 

setMasterInstrId :: InstrId -> GE ()
setMasterInstrId a = onMasterInstrId $ put a


----------------------------------------------------------------------
-- state modifiers

withOptions :: (Options -> a) -> GE a
withOptions f = GE $ asks f

modifyHistory :: (History -> History) -> GE ()
modifyHistory f = GE $ ReaderT $ \_ -> modify f

-- update fields

onHistory :: (History -> a) -> (a -> History -> History) -> State a b -> GE b
onHistory getter setter st = GE $ ReaderT $ \_ -> StateT $ \history -> 
    let (res, s1) = runState st (getter history)
    in  return (res, setter s1 history) 

type UpdField a b = State a b -> GE b

onUserInstr0 :: UpdField InstrBody a
onUserInstr0 = onHistory userInstr0 (\a h -> h { userInstr0 = a })

onInstr :: UpdField Instrs a
onInstr = onHistory instrs (\a h -> h { instrs = a })

onTotalDur :: UpdField (Maybe TotalDur) a
onTotalDur = onHistory totalDur (\a h -> h { totalDur = a })

onGlobals :: UpdField Globals a
onGlobals = onHistory globals (\a h -> h { globals = a })

onLocals :: UpdField Locals a
onLocals = onHistory locals (\a h -> h { locals = a })

onMidis :: UpdField [MidiAssign] a
onMidis = onHistory midis (\a h -> h { midis = a })

onMasterInstrId :: UpdField InstrId a
onMasterInstrId = onHistory masterInstrId (\a h -> h { masterInstrId = a })

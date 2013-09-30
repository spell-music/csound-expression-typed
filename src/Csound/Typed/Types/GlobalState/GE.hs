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
    , totalDur      :: Maybe TotalDur
    , masterInstrId :: InstrId
    , instr0        :: InstrBody }
 
instance Default History where
    def = History def def def def def def (intInstrId 0) (return ())

data TotalDur = NumDur Double | InfiniteDur

getTotalDur :: Options -> (Maybe TotalDur) -> Double
getTotalDur opt = toDouble . maybe InfiniteDur id  
    where 
        toDouble x = case x of
            NumDur d    -> d
            InfiniteDur -> setInfiniteDur opt

unsafePerformGE :: GE a -> a
unsafePerformGE = undefined

saveStr :: String -> GE E
saveStr = fmap prim . onStringMap . newString
    where onStringMap = onHistory stringMap (\val h -> h{ stringMap = val })

saveGen :: Gen -> GE E
saveGen = onGenMap . newGen
    where onGenMap = onHistory genMap (\val h -> h{ genMap = val })

setDurationToInfinite :: GE ()
setDurationToInfinite = onTotalDur $ put (Just InfiniteDur)

setDuration :: Double -> GE ()
setDuration = onTotalDur . put . Just . NumDur

type Channel = Int
data MidiType = Massign | Pgmassign (Maybe Int)
data Msg = Msg
data MidiAssign = MidiAssign MidiType Channel InstrId

saveMidi :: MidiAssign -> GE ()
saveMidi = undefined

setInstr0 :: Int -> GE ()
setInstr0 arity = (onInstr0 . put =<< ) $ fmap sequence_ $ sequence 
    [ withOptions setGlobalParams
    , onGlobals $ gets initGlobals 
    , return chnUpdateUdo ]
    where
        setGlobalParams opt = do
            setSr       $ setSampleRate opt
            setKsmps    $ setBlockSize opt
            setNchnls   arity
            setZeroDbfs 1
            maybe (return ()) (seed . double) $ setSeed opt

        seed = dep_ . opcs "seed" [(Xr, [Ir])] . return

        initGlobals = varsInits . globalsVars
            

getSysExpr :: GE (Dep ())
getSysExpr = fmap sequence_ $ sequence [ onGlobals $ clearGlobals ]    
    where
        clearGlobals :: State Globals (Dep ())
        clearGlobals = gets $ mapM_ (flip writeVar 0) . globalsClearable 

setMasterInstrId :: InstrId -> GE ()
setMasterInstrId a = onMasterInstrId $ put a

onInstrIO :: StateT Instrs IO a -> GE a
onInstrIO st = GE $ ReaderT $ \_ -> StateT $ \history -> do
    (res, instrs1) <- runStateT st (instrs history)
    return $ (res, history { instrs = instrs1 })
   

type UpdField a b = State a b -> GE b

onInstr0 :: UpdField InstrBody a
onInstr0 = onHistory instr0 (\a h -> h { instr0 = a })

onInstr :: UpdField Instrs a
onInstr = onHistory instrs (\a h -> h { instrs = a })

onTotalDur :: UpdField (Maybe TotalDur) a
onTotalDur = onHistory totalDur (\a h -> h { totalDur = a })

onGlobals :: UpdField Globals a
onGlobals = onHistory globals (\a h -> h { globals = a })

onLocals :: UpdField Locals a
onLocals = onHistory locals (\a h -> h { locals = a })

onMasterInstrId :: UpdField InstrId a
onMasterInstrId = onHistory masterInstrId (\a h -> h { masterInstrId = a })

onHistory :: (History -> a) -> (a -> History -> History) -> State a b -> GE b
onHistory getter setter st = GE $ ReaderT $ \_ -> StateT $ \history -> 
    let (res, s1) = runState st (getter history)
    in  return (res, setter s1 history) 

withOptions :: (Options -> a) -> GE a
withOptions f = GE $ asks f



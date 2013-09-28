module Csound.Typed.Types.GlobalState where

import Control.Applicative
import Control.Monad
import qualified Data.IntMap as IM
import Data.Default

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Csound.Dynamic 
import Csound.Dynamic.Control

newtype SE a = SE { unSE :: State (Maybe (GE E)) a }

instance Functor SE where
    fmap f = SE . fmap f . unSE

instance Applicative SE where
    pure = return
    (<*>) = ap

instance Monad SE where
    return = SE . return
    ma >>= mf = SE $ unSE ma >>= unSE . mf

runSE :: SE a -> (a, Maybe (GE E))
runSE a = runState (unSE a) Nothing

fromDep :: GE (Dep a) -> SE (GE a)
fromDep ma = SE $ state $ \s -> 
    case s of
        Nothing -> 
            let pairGE = do
                    a <- ma
                    return $ runState (unDep a) Nothing
            in (fmap fst pairGE, Just $ fmap (maybe emptyE id . snd) pairGE)
        Just mx ->
            let pairGE = do
                    a <- ma
                    x <- mx
                    return $ runState (unDep a) (Just x)
            in (fmap fst pairGE, Just $ fmap (maybe emptyE id . snd) pairGE)

fromDep_ :: GE (Dep ()) -> SE ()
fromDep_ = fmap (const ()) . fromDep
            
evalSE :: SE a -> a
evalSE = fst . runSE

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

data Options = Options 
    { setFlags          :: String
    , setSampleRate     :: Int
    , setBlockSize      :: Int    
    , setSeed           :: Maybe Double
    , setInfiniteDur    :: Double
    , setTabFi          :: TabFi }
   
instance Default Options where
    def = Options 
        { setFlags      = "-d"
        , setSampleRate = 44100
        , setBlockSize  = 64
        , setSeed       = Nothing
        , setInfiniteDur = 7 * 24 * 60 * 60  -- a week
        , setTabFi      = fineFi 13 [(idLins, 11), (idExps, 11), (idConsts, 9), (idSplines, 11), (idStartEnds, 12)] }
        where 
            idLins = 7
            idExps = 5
            idConsts = 17
            idSplines = 8
            idStartEnds = 16

-- | Table size fidelity (how many points in the table by default).
data TabFi = TabFi
    { tabFiBase   :: Int
    , tabFiGens   :: IM.IntMap Int }

-- | Sets different table size for different GEN-routines. 
--
-- > fineFi n ps 
--
-- where 
-- 
-- * @n@ is the default value for table size (size is a @n@ power of 2) for all gen routines that are not listed in the next argument @ps@.
--
-- * @ps@ is a list of pairs @(genRoutineId, tableSizeDegreeOf2)@ that sets the given table size for a 
--   given GEN-routine.
--
-- with this function we can set lower table sizes for tables that are usually used in the envelopes.
fineFi :: Int -> [(Int, Int)] -> TabFi
fineFi n xs = TabFi n (IM.fromList xs)

-- | Sets the same table size for all tables. 
--
-- > coarseFi n
--
-- where @n@  is a degree of 2. For example, @n = 10@ sets size to 1024 points for all tables by default.
coarseFi :: Int -> TabFi
coarseFi n = TabFi n IM.empty

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

-- smth is definetly wrong, we'll see..
genExecSG :: Maybe (GE E) -> SE (GE (Dep ())) -> GE (Dep ())
genExecSG s0 (SE x) = do
    res  <- mres
    expr <- case ms of
        Nothing -> return Nothing
        Just y  -> fmap Just y
    return $ Dep $ put $ snd $ runState (unDep res) expr
    where 
        (mres, ms) = runState x s0

execSG :: SE (GE (Dep ())) -> GE (Dep ())
execSG = genExecSG Nothing

execSG2 :: Dep (SE (GE (Dep ()))) -> GE (Dep ())
execSG2 (Dep x) = genExecSG (fmap return s) y
    where (y, s) = runState x Nothing

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



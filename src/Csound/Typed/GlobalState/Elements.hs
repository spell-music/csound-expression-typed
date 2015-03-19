{-# Language DeriveFunctor #-}
module Csound.Typed.GlobalState.Elements(
    -- * Identifiers
    IdMap(..), saveId, newIdMapId,
    -- ** Gens
    GenMap, newGen, newGenId,
    -- Sf2
    SfFluid(..), SfSpec(..), SfMap, newSf, sfVar, renderSf,
    -- ** Band-limited waveforms
    BandLimited(..), BandLimitedMap, 
    saveBandLimited, renderBandLimited,
    readBandLimited, readBandLimitedConstCps,
    -- ** String arguments
    StringMap, newString,
    -- * Midi
    MidiType(..), Channel, MidiMap, MidiKey(..), saveMidiInstr,
    -- * Global variables
    Globals(..), newPersistentGlobalVar, newClearableGlobalVar, 
    renderGlobals,
    -- * Instruments
    Instrs(..), saveInstr, CacheName, makeCacheName, saveCachedInstr, getInstrIds,
    -- * Src
    InstrBody, getIn, sendOut, sendChn, sendGlobal, chnPargId,
    Event(..),
    ChnRef(..), chnRefFromParg, chnRefAlloc, readChn, writeChn, chnUpdateUdo,
    subinstr, subinstr_, event_i, event, safeOut, autoOff, changed
) where


import Control.Monad.Trans.State.Strict
import Control.Monad(zipWithM_)
import Data.Default
import qualified Data.Map as M

import qualified System.Mem.StableName.Dynamic as DM
import qualified System.Mem.StableName.Dynamic.Map as DM

import Csound.Dynamic.Types
import Csound.Dynamic.Build
import Csound.Dynamic.Build.Numeric()

import Csound.Typed.GlobalState.Opcodes

-- tables of identifiers

data IdMap a = IdMap
    { idMapContent :: M.Map a Int
    , idMapNewId   :: Int }

instance Default (IdMap a) where   
    def = IdMap def 1

saveId :: Ord a => a -> State (IdMap a) Int
saveId a = state $ \s -> 
    case M.lookup a (idMapContent s) of
        Nothing -> 
            let newId = idMapNewId s
                s1    = s{ idMapContent = M.insert a newId (idMapContent s)
                         , idMapNewId = succ newId }    
            in  (newId, s1)
        Just n  -> (n, s)

newIdMapId :: State (IdMap a) Int
newIdMapId = state $ \s -> 
    let newId = idMapNewId s
        s1 = s { idMapNewId = succ newId } 
    in  (newId, s1)

-- gens

type GenMap = IdMap Gen

newGen :: Gen -> State GenMap E
newGen = fmap int . saveId

newGenId :: State GenMap Int
newGenId = newIdMapId

-- strings

type StringMap = IdMap String

newString :: String -> State StringMap Prim
newString = fmap PrimInt . saveId

-- sf

data SfFluid = SfFluid 
    { sfId   :: Int
    , sfVars :: [Var] }

data SfSpec = SfSpec
    { sfName    :: String
    , sfBank    :: Int
    , sfProgram :: Int 
    } deriving (Eq, Ord, Show)

type SfMap = IdMap SfSpec

newSf :: SfSpec -> State SfMap Int
newSf = saveId

sfVar :: Int -> E
sfVar n = readOnlyVar (VarVerbatim Ir $ sfEngineName n)

sfEngineName :: Int -> String
sfEngineName n = "gi_Sf_engine_" ++ show n

sfInstrName :: Int -> String
sfInstrName n = "i_Sf_instr_" ++ show n

renderSf :: Monad m => SfSpec -> Int -> DepT m ()
renderSf (SfSpec name bank prog) n = verbatim $ 
    engineStr ++ "\n" ++
    loadStr   ++ "\n" ++
    selectProgStr ++ "\n"
    where 
        engineStr = engineName ++ " fluidEngine"
        loadStr   = insName ++ " fluidLoad \"" ++ name ++ "\", " ++  engineName ++ ", 1"
        selectProgStr = "fluidProgramSelect " ++ engineName ++ ", 1, " ++ insName 
            ++ ", " ++ show bank ++ ", " ++ show prog

        engineName = sfEngineName n
        insName    = sfInstrName n

-- band-limited waveforms (used with vco2init)

data BandLimited = Saw | Pulse | Square | Triangle | IntegratedSaw | UserGen Gen
    deriving (Eq, Ord)

type BandLimitedMap = M.Map BandLimited Int

saveBandLimited :: BandLimited -> State (GenMap, BandLimitedMap) Int
saveBandLimited x = case x of
    Saw             -> simpleWave 1  0
    IntegratedSaw   -> simpleWave 2  1
    Pulse           -> simpleWave 4  2
    Square          -> simpleWave 8  3
    Triangle        -> simpleWave 16 4
    UserGen _       -> userGen 
    where
        simpleWave writeId readId = state $ \s@(genMap, blMap) ->
            if (M.member x blMap) 
                then (readId, s)
                else (readId, (genMap, M.insert x writeId blMap))

        userGen = state $ \s@(genMap, blMap) -> case M.lookup x blMap of
            Just n  -> (n, s)
            Nothing -> 
                let (newId, genMap1) = runState newGenId genMap
                    blMap1 = M.insert x newId blMap
                in  (negate newId, (genMap1, blMap1))        

renderBandLimited :: Monad m => GenMap -> BandLimitedMap -> DepT m ()
renderBandLimited genMap blMap = case M.toList blMap of
    []  -> return ()
    as  -> render (idMapNewId genMap) (getUserGens as) as
    where 
        render n gens vcos = do
            mapM_ renderGen gens
            renderFirstVco n (head vcos)
            mapM_ renderTailVco (tail vcos)        

        getUserGens as = phi =<< as
            where phi (x, gId) = case x of
                        UserGen g   -> [(g, gId)]
                        _           -> []
       
        renderGen (g, n) = toDummy $ ftgen (int n) g

        renderFirstVco n x = renderVco (int n) x
        renderTailVco x = renderVco (readOnlyVar vcoVar) x

        renderVco ftId (wave, waveId) = toVcoVar $ vco2init $ case wave of
            UserGen _   -> [ int waveId, ftId, 1.05, -1, -1, int $ negate waveId ]
            _           -> [ int waveId, ftId ]

        vcoVar = dummyVar
        toVcoVar = toDummy

        dummyVar = Var LocalVar Ir "ft" 

        toDummy = writeVar dummyVar

readBandLimited :: Int -> E -> E
readBandLimited n cps = oscilikt 1 cps (vco2ft cps (int n))

readBandLimitedConstCps :: Int -> E -> E
readBandLimitedConstCps n cps = oscili 1 cps (vco2ift cps (int n))

----------------------------------------------------------
-- Midi

type Channel = Int

data MidiType = Massign | Pgmassign (Maybe Int)
    deriving (Show, Eq, Ord)

data MidiKey = MidiKey MidiType Channel
    deriving (Show, Eq, Ord)

type MidiMap m = M.Map MidiKey (DepT m ())

saveMidiInstr :: Monad m => MidiType -> Channel -> DepT m () -> MidiMap m -> MidiMap m
saveMidiInstr ty chn body = M.insertWith (flip (>>)) (MidiKey ty chn) body

-- global variables

data Globals = Globals
    { globalsNewId  :: Int
    , globalsVars   :: [AllocVar] }

data AllocVar = AllocVar 
    { allocVarType     :: GlobalVarType 
    , allocVar         :: Var
    , allocVarInit     :: E 
    }

data GlobalVarType = PersistentGlobalVar | ClearableGlobalVar
    deriving (Eq)

instance Default Globals where
    def = Globals def def

newGlobalVar :: GlobalVarType -> Rate -> E -> State Globals Var
newGlobalVar ty rate initVal = state $ \s ->
    let newId = globalsNewId s        
        var   = Var GlobalVar rate ('g' : show newId) 
        s1    = s { globalsNewId = succ newId
                  , globalsVars  = AllocVar ty var initVal : globalsVars s }
    in  (var, s1)

newPersistentGlobalVar :: Rate -> E -> State Globals Var
newPersistentGlobalVar = newGlobalVar PersistentGlobalVar

newClearableGlobalVar :: Rate -> E -> State Globals Var
newClearableGlobalVar = newGlobalVar ClearableGlobalVar
 
renderGlobals :: Monad m => Globals -> (DepT m (), DepT m ())
renderGlobals a = (initAll, clear)
    where
        initAll = mapM_ (\x -> initVar (allocVar x) (allocVarInit x)) gs
        clear   = mapM_ (\x -> writeVar (allocVar x) (allocVarInit x)) clearable
        clearable = filter ((== ClearableGlobalVar) . allocVarType) gs
        gs = globalsVars a

-----------------------------------------------------------------
-- instrs

data Instrs = Instrs
    { instrsCache   :: DM.Map InstrId
    , instrsNewId   :: Int
    , instrsContent :: [(InstrId, InstrBody)]
    }

instance Default Instrs where
    def = Instrs DM.empty 18 []

type CacheName = DM.DynamicStableName

makeCacheName :: a -> IO CacheName
makeCacheName = DM.makeDynamicStableName 

getInstrIds :: Instrs -> [InstrId]
getInstrIds = fmap fst . instrsContent

-----------------------------------------------------------------
--
saveCachedInstr :: CacheName -> InstrBody -> State Instrs InstrId 
saveCachedInstr name body = state $ \s -> 
    case DM.lookup name $ instrsCache s of
        Just n  -> (n, s)
        Nothing -> 
            let newId   = instrsNewId s
                s1      = s { instrsCache   = DM.insert name (intInstrId newId) $ instrsCache s
                            , instrsNewId   = succ newId
                            , instrsContent = (intInstrId newId, body) : instrsContent s }
            in  (intInstrId newId, s1)

saveInstr :: InstrBody -> State Instrs InstrId
saveInstr body = state $ \s ->
    let newId   = instrsNewId s
        s1      = s { instrsNewId   = succ newId
                    , instrsContent = (intInstrId newId, body) : instrsContent s }
    in  (intInstrId newId, s1)

-----------------------------------------------------------------
-- sound sources

getIn :: Monad m => Int -> DepT m [E]
getIn arity
    | arity == 0    = return []
    | otherwise     = ($ arity ) $ mdepT $ mopcs name (replicate arity Ar, []) []
    where
        name
            | arity == 1 = "in"
            | arity == 2 = "ins"
            | arity == 4 = "inq"
            | arity == 6 = "inh"
            | arity == 8 = "ino"
            | arity == 16 = "inx"
            | arity == 32 = "in32"
            | otherwise = "ins"

sendOut :: Monad m => Int -> [E] -> DepT m ()
sendOut arity sigs 
    | arity == 0    = return ()
    | otherwise     = do
        vars <- newLocalVars (replicate arity Ar) (return $ replicate arity 0)
        zipWithM_ writeVar vars sigs
        vals <- mapM readVar vars
        depT_ $ opcsNoInlineArgs name [(Xr, replicate arity Ar)] vals
    where
        name            
            | arity == 1 = "out"
            | arity == 2 = "outs"
            | arity == 4 = "outq"
            | arity == 6 = "outh"
            | arity == 8 = "outo"
            | arity == 16 = "outx"
            | arity == 32 = "out32"
            | otherwise = "outc"

sendGlobal :: Monad m => Int -> [E] -> State Globals ([E], DepT m ())
sendGlobal arityOuts sigs = do
    vars <- mapM (uncurry newClearableGlobalVar) $ replicate arityOuts (Ar, 0)
    return (fmap readOnlyVar vars, zipWithM_ (appendVarBy (+)) vars sigs)

sendChn :: Monad m => Int -> Int -> [E] -> DepT m ()
sendChn arityIns arityOuts sigs = writeChn (chnRefFromParg (chnPargId arityIns) arityOuts) sigs

chnPargId :: Int -> Int
chnPargId arityIns = 4 + arityIns

-- guis




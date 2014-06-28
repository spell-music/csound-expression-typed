module Csound.Typed.GlobalState.Opcodes(
    sprintf,
    -- * channel opcodes
    ChnRef(..), chnRefFromParg, chnRefAlloc, readChn, writeChn, 
    chnUpdateUdo,
    -- * trigger an instrument
    Event(..), event, event_i, appendChn, subinstr, subinstr_, changed,
    -- * output
    out, outs, safeOut, autoOff, turnoff, turnoff2, exitnow,
    -- * vco2
    oscili, oscilikt, vco2ft, vco2ift, vco2init, ftgen,
    -- * times
    times
) where

import Control.Monad(zipWithM_)
import Data.Boolean

import Csound.Dynamic

-- channels

data ChnRef = ChnRef 
    { chnRefId      :: E
    , chnRefNames   :: [E] }

chnRefFromParg :: Int -> Int -> ChnRef
chnRefFromParg pargId arity = ChnRef (pn pargId) $ fmap (flip chnName (pn pargId)) [1 .. arity]

chnRefAlloc :: Monad m => Int -> DepT m ChnRef
chnRefAlloc arity = do
    chnId <- freeChn
    return $ ChnRef chnId $ fmap (flip chnName chnId) [1 .. arity]
    
readChn :: Monad m => ChnRef -> DepT m [E]
readChn ref = do
    res <- mapM chnget $ chnRefNames ref
    clearChn ref
    return res

writeChn :: Monad m => ChnRef -> [E] -> DepT m ()
writeChn ref sigs = zipWithM_ chnmix sigs $ chnRefNames ref
    
clearChn :: Monad m => ChnRef -> DepT m ()
clearChn = mapM_ chnclear . chnRefNames

-- | 
-- > chnName outputPortNumber freeChnId
chnName :: Int -> E -> E
chnName name chnId = sprintf formatString [chnId]
    where formatString = str $ 'p' : show name ++ "_" ++ "%d"

sprintf :: E -> [E] -> E
sprintf a as = opcs "sprintf" [(Sr, Sr:repeat Ir)] (a:as)

chnmix :: Monad m => E -> E -> DepT m ()
chnmix asig name = do
    var <- newLocalVar Ar (return 0)
    writeVar var asig
    val <- readVar var
    depT_ $ opcsNoInlineArgs "chnmix" [(Xr, [Ar, Sr])] [val, name]

chnget :: Monad m => E -> DepT m E
chnget name = depT $ opcs "chnget" [(Ar, [Sr])] [name]

chnclear :: Monad m => E -> DepT m ()
chnclear name = depT_ $ opcs "chnclear" [(Xr, [Sr])] [name]

chnUpdateUdo :: Monad m => DepT m ()
chnUpdateUdo = verbatim $ unlines [
    "giPort init 1",
    "opcode " ++ chnUpdateOpcodeName ++ ", i, 0",
    "xout giPort",
    "giPort = giPort + 1",
    "endop"]


chnUpdateOpcodeName :: String
chnUpdateOpcodeName = "FreePort"

freeChn :: Monad m => DepT m E
freeChn = depT $ opcs chnUpdateOpcodeName [(Ir, [])] []

-- trigger

data Event = Event
    { eventInstrId  :: InstrId
    , eventStart    :: E
    , eventDur      :: E
    , eventArgs     :: [E] }

event :: Monad m => Event -> DepT m ()
event = eventBy "event" Kr

event_i :: Monad m => Event -> DepT m ()
event_i = eventBy "event_i" Ir

eventBy :: Monad m => String -> Rate -> Event -> DepT m ()
eventBy name rate a = depT_ $ opcs name [(Xr, repeat rate)] 
    (str "i" : (prim (PrimInstrId $ eventInstrId a)) : (eventStart a) : (eventDur a) : (eventArgs a))

appendChn :: E -> Event -> Event
appendChn chn a = a { eventArgs = eventArgs a ++ [chn] }

subinstr :: Int -> InstrId -> [E] -> [E]
subinstr outArity instrId args = ( $ outArity) $ mopcs "subinstr" 
    (repeat Ar, Ir : repeat Kr) 
    (prim (PrimInstrId instrId) : args)

subinstr_ :: Monad m => InstrId -> [E] -> DepT m ()
subinstr_ instrId args = depT_ $ head $ ($ 1) $  mopcs "subinstr" 
    (repeat Ar, Ir : repeat Kr)
    (prim (PrimInstrId instrId) : args)

changed :: E -> E
changed x = opcs "changed" [(Kr, [Kr])] [x]

-- output

out :: Monad m => E -> DepT m ()
out a = depT_ $ opcsNoInlineArgs "out" [(Xr, [Ar])] [a]

outs :: Monad m => [E] -> DepT m ()
outs as = depT_ $ opcsNoInlineArgs "outs" [(Xr, repeat Ar)] as

-- safe out

-- clipps values by 0dbfs
safeOut :: Double -> [E] -> [E] 
safeOut gainLevel = fmap (( * double gainLevel) . clip)
    where clip x = opcs "clip" [(Ar, [Ar, Ir, Ir])] [x, 0, readOnlyVar (VarVerbatim Ir "0dbfs")]

autoOff :: Monad m => E -> [E] -> DepT m [E]
autoOff dt a = do
    ihold    
    when1 (trig a)
        turnoff
    return a
    where
        trig = (<* eps) . (env + ) . setRate Kr . flip follow dt . l2 

        eps = 1e-5

        l2 :: [E] -> E
        l2 xs = sqrt $ sum $ zipWith (*) xs xs 

        env = linseg [1, dt/2, 1, dt/2, 0, 1, 0]

follow :: E -> E -> E
follow asig dt = opcs "follow" [(Ar, [Ar, Ir])] [asig, dt]

turnoff :: Monad m => DepT m ()
turnoff = depT_ $ opcs "turnoff" [(Xr, [])] []

turnoff2 :: Monad m => E -> DepT m ()
turnoff2 instrId = depT_ $ opcs "turnoff2" [(Xr, [Ir, Ir, Ir])] [instrId, 0, 0]

exitnow :: Monad m => DepT m ()
exitnow = depT_ $ opcs "exitnow" [(Xr, [])] []

ihold :: Monad m => DepT m ()
ihold = depT_ $ opcs "ihold" [(Xr, [])] []

linseg :: [E] -> E
linseg = opcs "linseg" [(Kr, repeat Ir)]

-- vco2

-- ares oscilikt xamp, xcps, kfn [, iphs] [, istor]
-- kres oscilikt kamp, kcps, kfn [, iphs] [, istor]
oscilikt :: E -> E -> E -> E
oscilikt amp cps fn = opcs "oscilikt" 
    [ (Ar, [Xr, Xr, Kr, Ir, Ir])
    , (Kr, [Kr, Kr, Kr, Ir, Ir])]     
    [amp, cps, fn]

-- ares oscili xamp, xcps, ifn [, iphs]
-- kres oscili kamp, kcps, ifn [, iphs]
oscili :: E -> E -> E -> E
oscili amp cps fn = opcs "oscili"
    [ (Ar, [Xr, Xr, Ir, Ir, Ir])
    , (Kr, [Kr, Kr, Ir, Ir, Ir])]     
    [amp, cps, fn]

-- kfn vco2ft kcps, iwave [, inyx]
vco2ft :: E -> E -> E
vco2ft cps iwave = opcs "vco2ft" [(Kr, [Kr, Ir, Ir])] [cps, iwave]

vco2ift :: E -> E -> E
vco2ift cps iwave = opcs "vco2ift" [(Kr, [Ir, Ir, Ir])] [cps, iwave]

ftgen :: E -> Gen -> E
ftgen n g = opcs "ftgen" [(Ir, repeat Ir)]
    $ [n, 0, int $ genSize g, int $ genId g]
    ++ (maybe [] (return . str) $ genFile g)
    ++ (fmap double $ genArgs g)

vco2init :: [E] -> E
vco2init = opcs "vco2init" [(Ir, repeat Ir)]

----------------------
-- times

times :: Monad m => DepT m E
times = depT $ opcs "times" [(Ir, []), (Kr, [])] []


{-# Language ScopedTypeVariables #-}
module Csound.Typed.Control.Api(
    trigByName, trigByName_,
    trigByNameMidi, trigByNameMidi_,
    namedMonoMsg
) where

import Data.Boolean
import Control.Monad.Trans.Class

import qualified Csound.Dynamic as D
import Csound.Dynamic(Rate(..), opcs, depT_)
import Data.Boolean((==*), (>*), ifB)

import Csound.Typed.Types
import Csound.Typed.Control.Ref
import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Opcodes(eventi, Event(..), turnoff, port, downsamp)

import Csound.Typed.Plugins.TabQueue

-- | Creates an instrument that can be triggered by name with Csound API.
-- The arguments are determined from the structure of the input for the instrument.
--
-- With Csound API we can send messages 
--
-- > i "name" time duration arg1 arg2 arg3
trigByName_ :: Arg a => String -> (a -> SE ()) -> SE ()
trigByName_ name instr = geToSe $ saveNamedInstr name =<< (execSE $ instr toArg)

-- | Creates an instrument that can be triggered by name with Csound API.
-- The arguments are determined from the structure of the input for the instrument.
trigByName  :: (Arg a, Sigs b) => String -> (a -> SE b) -> SE b
trigByName name instr = do
    ref <- newClearableGlobalRef 0
    trigByName_ name (go ref)
    readRef ref    
    where go ref x = mixRef ref =<< instr x

trigByNameMidi_ :: forall a . Arg a => String -> ((D, D, a) -> SE ()) -> SE ()
trigByNameMidi_ name instr = do
    instrId <- geToSe $ saveInstr (instr toArg)
    trigByName_ name (go instrId)
    where
        go :: D.InstrId -> (D, D, D, a) -> SE ()
        go instrId (noteFlag, pch, vol, other) = fromDep_ $ hideGEinDep $ do
            pchExpr      <- toGE pch
            let instrIdExpr = D.instrIdE instrId + pchExpr / 1000
            noteFlagExpr <- toGE noteFlag
            args <- fromTuple (pch, vol, other)            
            return $ do
                    D.when1 (noteFlagExpr ==* 1) $ do
                        eventi (Event instrIdExpr 0 (-1) args)
                    D.when1 (noteFlagExpr ==* 0) $ do
                        eventi (Event (negate instrIdExpr) 0 0 args)
                    turnoff

-- | Creates an instrument that behaves much like a midi instrument but can be triggered
-- with Csound API. The input is a tuple of:
--
-- > (pitchKey, volumeKey, miscArguments)
--
-- But when we use it with Csound API we should pass another argument (as the first argument or p4):
--
-- > i "name" time duration note_on_off pitch volume arg1 arg2 arg3
--
-- where @note_on_off_flag@ equals to 1 if it's note on message and equals to zero 
-- if it's a note off message.
-- @pitchKey@  and @volumeKey@ are standard midi integer numbers (0 to 127).
-- The fourth argument is for some auxilliary parameters.
--
-- With Csound API we can send messages 
trigByNameMidi  :: (Arg a, Sigs b) => String -> ((D, D, a) -> SE b) -> SE b
trigByNameMidi name instr = do
    ref <- newClearableGlobalRef 0
    trigByNameMidi_ name (go ref)
    readRef ref    
    where go ref x = mixRef ref =<< instr x

namedMonoMsg :: D -> D -> String -> SE (Sig, Sig)
namedMonoMsg portTime relTime name = do
    refPch <- newGlobalRef 0
    refVol <- newGlobalRef 0
    tab <- newGlobalTab 24
    let onFlag = tabQueue2_hasElements tab
    trigByNameMidiCbk name (onNote tab) (offNote tab)
    when1 onFlag $ do
        let (pch, vol) = tabQueue2_readLastElement tab
        writeRef refPch pch
        writeRef refVol vol
    when1 (notB onFlag) $ do
        writeRef refVol 0
    pchKey <- readRef refPch
    volKey <- readRef refVol
    let resStatus = ifB onFlag 1 0
    return (port' (downsamp' volKey) portTime * port' resStatus relTime,  port' (downsamp' pchKey) portTime)
    where        
        onNote = tabQueue2_append        
        offNote tab (pch, vol) = tabQueue2_delete tab pch

trigByNameMidiCbk :: String -> ((D, D) -> SE ())  -> ((D, D) -> SE ()) -> SE ()
trigByNameMidiCbk name noteOn noteOff = 
    trigByName_ name go
    where
        go :: (D, D, D) -> SE ()
        go (noteFlag, pch, vol) = do            
            whenD1 (noteFlag ==* 1) $ noteOn (pch, vol)
            whenD1 (noteFlag ==* 0) $ noteOff (pch, vol)
            SE turnoff

{-
namedMonoMsg :: D -> D -> String -> SE (Sig, Sig)
namedMonoMsg portTime relTime name = do
    ref <- newGlobalRef (0, 0, 0)  
    tab <- newGlobalTab 16  
    trigByName_ name (go tab ref)
    (pchKey, volKey, status) <- readRef ref
    let resStatus = ifB (downsamp' status ==* 0) 0 1
    return (port' (downsamp' volKey) portTime * port' resStatus relTime,  port' (downsamp' pchKey) portTime)
    where
        go :: Tab -> Ref (Sig, Sig, Sig) -> (D, D, D) -> SE ()
        go pitchTab ref (noteFlag, pch, vol) = do
            (_, volOld, status) <- readRef ref
            when1 (sig noteFlag ==* 1) $ do    
                tabw (sig pch) status pitchTab
                writeRef ref (sig pch, sig vol, status + 1)
            when1 (sig noteFlag ==* 0) $ do
                when1 (status ==* 0) $ do
                    writeRef ref (sig pch, volOld, status - 1)
                when1 (status >* 0) $ do
                    let pchOld = tab (status - 1) pitchTab
                    writeRef ref (pchOld, volOld, status - 1)

            fromDep_ turnoff
-}
port' :: Sig -> D -> Sig
port' a b = fromGE $ do
    a' <- toGE a
    b' <- toGE b
    return $ port a' b'

downsamp' :: Sig -> Sig
downsamp' a = fromGE $ do
    a' <- toGE a    
    return $ downsamp a'

-- | 
-- Fast table opcodes.
--
-- Fast table opcodes. Faster than
--     table and
--     tablew because don't
--     allow wrap-around and limit and don't check index validity. Have
--     been implemented in order to provide fast access to
--     arrays. Support non-power of two tables (can be generated by any
--     GEN function by giving a negative length value).
--
-- >  tabw  ksig, kndx, ifn [,ixmode]
-- >  tabw  asig, andx, ifn [,ixmode]
--
-- csound doc: <http://www.csounds.com/manual/html/tab.html>
tabw ::  Sig -> Sig -> Tab -> SE ()
tabw b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
    where f a1 a2 a3 = opcs "tabw" [(Xr,[Kr,Kr,Ir,Ir])] [a1,a2,a3]


-- | 
-- Fast table opcodes.
--
-- Fast table opcodes. Faster than
--     table and
--     tablew because don't
--     allow wrap-around and limit and don't check index validity. Have
--     been implemented in order to provide fast access to
--     arrays. Support non-power of two tables (can be generated by any
--     GEN function by giving a negative length value).
--
-- > kr  tab  kndx, ifn[, ixmode]
-- > ar  tab  xndx, ifn[, ixmode]
--
-- csound doc: <http://www.csounds.com/manual/html/tab.html>
tab ::  Sig -> Tab -> Sig
tab b1 b2 = Sig $ f <$> unSig b1 <*> unTab b2
    where f a1 a2 = opcs "tab" [(Kr,[Kr,Ir,Ir]),(Ar,[Xr,Ir,Ir])] [a1,a2]
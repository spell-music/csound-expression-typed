{-# Language ScopedTypeVariables #-}
module Csound.Typed.GlobalState.InstrApi where

import Control.Monad
import Control.Monad.Trans.Class

import Csound.Dynamic hiding (InstrId)
import Csound.Typed.GlobalState.Instr
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.Types.Tuple
import Csound.Typed.Types.Prim

import Csound.Typed.GlobalState.Opcodes(freeChn, chnName, chnget, chnset)
import qualified Csound.Typed.GlobalState.Opcodes as Opcodes(Event(..), event, eventi, turnoff2)

newtype Port a = Port { unPort :: GE E }

freePort :: forall a . Sigs a => SE (Port a)
freePort = SE $ fmap (Port . return) $ freeChn

instance Sigs a => Tuple (Port a) where
    tupleMethods = makeTupleMethods to from
        where
            to :: D -> Port a
            to =  Port . toGE 

            from :: Port a -> D
            from (Port e) = fromGE e

instance Sigs a => Arg (Port a) where

getNames :: forall a . Sigs a => Port a -> GE [E]
getNames (Port ref) = do
    idx <- ref
    return $ fmap (flip chnName idx) [1 .. (tupleArity ((error "No def here") :: a))]

readPort :: Sigs a => Port a -> SE a
readPort port = SE $ hideGEinDep $ do
    names <- getNames port
    return $ fmap (toTuple . return) $ mapM chnget names

writePort :: Sigs a => Port a -> a -> SE ()
writePort port a = SE $ do
    (names, values) <- lift getNamesAndValues
    zipWithM_ chnset names values
    where 
        getNamesAndValues = do
            names  <- getNames port
            values <- fromTuple a            
            return (names, values)

mixPort :: Sigs a => Port a -> a -> SE ()
mixPort p value = do
    st <- readPort p
    writePort p (st + value)

newtype InstrId a = InstrId { unInstrId :: GE E }

newInstr :: Arg a => (a -> SE ()) -> InstrId a
newInstr instr = InstrId $ fmap instrIdE $ saveInstr (instr toArg)

event :: Arg a => InstrId a -> (D,D,a) -> SE ()
event idx note = SE $ do
    e <- lift $ getEvent idx note
    Opcodes.event e
            
getEvent (InstrId idx) (start, dur, args) = do
            i <- idx
            s <- toGE start
            d <- toGE dur
            as <- fromTuple args
            return $ Opcodes.Event i s d as

eventi :: Arg a => InstrId a -> (D,D,a) -> SE ()
eventi idx note = SE $ do
    e <- lift $ getEvent idx note
    Opcodes.eventi e

turnoff2 :: InstrId a -> SE ()
turnoff2 (InstrId expr) = SE $ Opcodes.turnoff2 =<< lift expr


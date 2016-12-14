-- | Imperative csound instruments
module Csound.Typed.Control.InstrRef(
    InstrRef, newInstr, scheduleEvent, negateInstrRef, addFracInstrRef,
    newOutInstr, noteOn, noteOff
) where    

import Data.Default
import Csound.Dynamic(InstrId(..))
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Ref

data InstrFrac = InstrFrac 
    { instrFracValue :: D
    , instrFracSize  :: D    
    }

data InstrRef a = InstrRef 
    { instrRefMain :: D
    , instrRefFrac :: Maybe InstrFrac }

newInstr ::  (Arg a) => (a -> SE ()) -> SE (InstrRef a)
newInstr instr = geToSe $ fmap fromInstrId $ saveInstr $ instr toArg

scheduleEvent :: (Arg a) => InstrRef a -> D -> D -> a -> SE ()
scheduleEvent instrRef start end args = SE $ hideGEinDep $ fmap C.event $ C.Event <$> toGE (getInstrId instrRef) <*> toGE start <*> toGE end <*> toNote args

getInstrId :: InstrRef a -> D
getInstrId (InstrRef value frac) = value + maybe 0 fromFrac frac
    where
        fromFrac (InstrFrac value size) = (value * 10 + 1) / (size * 10)

negateInstrRef :: InstrRef a -> InstrRef a 
negateInstrRef ref = ref { instrRefMain = negate $ instrRefMain ref }

addFracInstrRef :: D -> D -> InstrRef a -> InstrRef a
addFracInstrRef maxSize value instrRef = instrRef { instrRefFrac = Just (InstrFrac value maxSize) }

fromInstrId :: InstrId -> InstrRef a
fromInstrId x = case x of
    InstrId frac ceil -> InstrRef (int ceil) Nothing
    InstrLabel _    -> error "No reference for string instrument id. (Csound.Typed.Control.Instr.hs: fromInstrId)"

newOutInstr :: (Arg a, Sigs b) => (a -> SE b) -> SE (InstrRef a, b)
newOutInstr f = do
    ref <- newClearableGlobalRef 0
    instrId <- newInstr $ \a -> mixRef ref =<< f a
    aout <- readRef ref
    return (instrId, aout)

noteOn :: (Arg a) => D -> D -> InstrRef a -> a -> SE ()
noteOn maxSize noteId instrId args = scheduleEvent (addFracInstrRef maxSize noteId instrId) 0 (-1) args

noteOff :: (Default a, Arg a) => D -> D -> InstrRef a -> SE () 
noteOff maxSize noteId instrId = scheduleEvent (negateInstrRef $ addFracInstrRef maxSize noteId instrId) 0 0.01 def
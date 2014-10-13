{-# Language TypeFamilies #-}
module Csound.Typed.Control.Sf2(
    Sf(..), unSf
  --  initSf, sfMidiNote, sfNote, sfMidi, sfMidin, sfPgmidi
) where

import Data.Boolean
import Data.Default
import Control.Monad

import qualified Csound.Dynamic as D
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types
import Csound.Typed.GlobalState

data Sf = Sf 
    { sfName :: String
    , sfBank :: Int
    , sfProg :: Int } 
    | SfId (GE E)
    
instance Val Sf where
    fromGE = SfId
    toGE   = unSf

unSf :: Sf -> GE E
unSf x = case x of
    SfId a -> a
    Sf name bank prog -> fmap D.int $ saveSf (SfSpec name bank prog)

instance Default Sf where 
    def = fromE 0

type instance BooleanOf Sf  = BoolD
instance IfB Sf where ifB = on3 ifB

{-
----------------------------------------------------
-- fluid synth

sfMidi :: Sf -> (Sig, Sig)
sfMidi = sfMidin 0

sfMidin :: Channel -> Sf -> (Sig, Sig)
sfMidin n = genMidi Massign n

sfPgmidi :: Maybe Int -> Channel -> Sf -> (Sig, Sig)
sfPgmidi mchn n = genMidi (Pgmassign mchn) n 

genMidi :: MidiType -> Channel -> Sf -> (Sig, Sig)
genMidi mtype chn sf = undefined

initSf :: String -> Int -> Int -> SE SfFluid
initSf fileName bank program = geToSe $ do
    engineId <- saveSf $ SfSpec fileName bank program
    vars <- mkSfOutput $ sfVar engineId
    return $ SfFluid engineId vars

sfMidiNote :: SfFluid -> D -> D -> SE (Sig, Sig)
sfMidiNote sf vel key = do
    triggerNote (sfEngineId sf) vel key
    return $ toTuple $ return $ fmap D.readOnlyVar $ sfVars sf

sfEngineId :: SfFluid -> E
sfEngineId = sfVar . sfId

sfNote :: SfFluid -> D -> D -> SE (Sig, Sig)
sfNote = undefined

fluidChannel :: Int
fluidChannel = 1

triggerNote :: E -> D -> D -> SE ()
triggerNote engineId ivel ikey = SE $ hideGEinDep $ do
    key <- toGE ikey
    vel <- toGE ivel
    return $ fluidNote engineId (D.int fluidChannel) key vel

fluidNote :: E -> E -> E -> E -> Dep ()
fluidNote engineId channel key vel = D.depT_ $ 
    D.opcs "fluidNote" [(D.Xr, replicate 4 D.Ir)] [engineId, channel, key, vel]

mkSfOutput:: E -> GE [D.Var]
mkSfOutput engineId = do
    vars <- onGlobals $ sequence $ replicate 2 (C.newClearableGlobalVar D.Ar 0)
    let expr = (SE . zipWithM_ (D.appendVarBy (+)) vars) =<< instr
    instrId <- saveInstr expr
    saveAlwaysOnInstr instrId
    return vars
    where instr = fluidOutInstr engineId

fluidOutInstr :: E -> SE [E]
fluidOutInstr engineId = return $ fluidOut engineId
   
fluidOut :: E -> [E]
fluidOut engineId = D.mopcs "fluidOut" ([D.Ar, D.Ar], [D.Ir]) [engineId] 2

-}

module Csound.Typed.Control.Sf2(
    sf2, sf2n, pgsf2        
) where

import Control.Monad

import qualified Csound.Dynamic as D
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types
import Csound.Typed.GlobalState

todo :: a
todo = undefined

fluidChannel :: Int
fluidChannel = 1

data Sf = Sf
    { sfName    :: String
    , sfBank    :: Int
    , sfProgram :: Int } 

sf2 :: String -> Int -> Int -> (Sig, Sig)
sf2 = sf2n 0

sf2n :: Channel -> String -> Int -> Int -> (Sig, Sig)
sf2n channel = genSf2 channel Massign

pgsf2 :: Maybe Int -> Channel -> String -> Int -> Int -> (Sig, Sig)
pgsf2 mchn n = genSf2 n (Pgmassign mchn)

genSf2 :: Int -> MidiType -> String -> Int -> Int -> (Sig, Sig)
genSf2 channel midiType fileName bank pgm = toTuple $ do
    engineId <- initSf (Sf fileName bank pgm)
    _ <- saveMidiInstr_ midiType channel $ triggerNote engineId
    readOutput engineId

initSf :: Sf -> GE E
initSf = todo

triggerNote :: E -> SE ()
triggerNote engineId = SE $ do
    ivel <- veloc
    ikey <- notnum
    fluidNote engineId (D.int fluidChannel) ikey ivel
        
readOutput :: E -> GE [E]
readOutput engineId = do
    vars <- onGlobals $ sequence $ replicate 2 (C.newClearableGlobalVar D.Ar 0)
    let expr = (SE . zipWithM_ (D.appendVarBy (+)) vars) =<< instr
    _ <- saveInstr expr
    return $ fmap D.readOnlyVar vars         
    where instr = fluidOutInstr engineId

fluidOutInstr :: E -> SE [E]
fluidOutInstr engineId = return $ fluidOut engineId

fluidNote :: E -> E -> E -> E -> Dep ()
fluidNote engineId channel key vel = D.depT_ $ 
    D.opcs "fluidNote" [(D.Xr, replicate 4 D.Ir)] [engineId, channel, key, vel]
   
fluidOut :: E -> [E]
fluidOut engineId = D.mopcs "fluidOut" ([D.Ar, D.Ar], [D.Ir]) [engineId] 2

veloc :: Dep E
veloc = D.depT $ D.opcs "veloc" [(D.Ir, [])] []

notnum :: Dep E
notnum = D.depT $ D.opcs "notnum" [(D.Ir, [])] []

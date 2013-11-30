module Csound.Typed.Render(
    renderOut, renderOutBy, 
    renderEff, renderEffBy,
    renderOut_, renderOutBy_, 
    -- * Options
    module Csound.Typed.GlobalState.Options,
    module Csound.Dynamic.Types.Flags
) where

import qualified Data.Map as M
import Data.Default
import Data.Maybe
import Data.Tuple
import Control.Monad

import Csound.Dynamic hiding (csdFlags)
import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Options
import Csound.Typed.Control.Instr
import Csound.Typed.Control(getIns)
import Csound.Dynamic.Types.Flags

import Csound.Typed.Gui.Gui(guiStmt)

toCsd :: Tuple a => Options -> SE a -> GE Csd
toCsd options sigs = do   
    saveMasterInstr (constArity sigs) (masterExp sigs)
    join $ withHistory $ renderHistory (outArity sigs) options

renderOut_ :: SE () -> IO String
renderOut_ = renderOutBy_ def 

renderOutBy_ :: Options -> SE () -> IO String
renderOutBy_ options sigs = evalGE options $ fmap renderCsd $ toCsd options (fmap (const unit) sigs)

renderOut :: Sigs a => SE a -> IO String
renderOut = renderOutBy def

renderOutBy :: Sigs a => Options -> SE a -> IO String
renderOutBy options sigs = evalGE options $ fmap renderCsd $ toCsd options sigs

renderEff :: (Sigs a, Sigs b) => (a -> SE b) -> IO String
renderEff = renderEffBy def

renderEffBy :: (Sigs a, Sigs b) => Options -> (a -> SE b) -> IO String
renderEffBy options eff = renderOutBy options $ eff =<< getIns

renderHistory :: Int -> Options -> History -> GE Csd
renderHistory nchnls opt hist = do
    instr0 <- execDepT $ getInstr0 nchnls opt hist
    let orc = Orc instr0 (fmap (uncurry Instr) $ instrsContent $ instrs hist)   
    return $ Csd flags orc sco
    where
        flags   = reactOnMidi hist $ csdFlags opt
        sco     = Sco (Just $ getTotalDur opt $ totalDur hist) 
                      (renderGens $ genMap hist) $
                      (fmap alwaysOn $ alwaysOnInstrs hist)

        renderGens = fmap swap . M.toList . idMapContent        

getInstr0 :: Int -> Options -> History -> Dep ()
getInstr0 nchnls opt hist = do
    globalConstants
    midiAssigns
    initGlobals
    renderBandLimited (genMap hist) (bandLimitedMap hist)
    userInstr0 hist
    chnUpdateUdo 
    guiStmt $ getPanels hist
    where
        globalConstants = do
            setSr       $ defSampleRate opt
            setKsmps    $ defBlockSize opt
            setNchnls   (max 1 nchnls)
            setZeroDbfs 1

        midiAssigns = mapM_ renderMidiAssign $ midis hist

        initGlobals = fst $ renderGlobals $ globals $ hist


reactOnMidi :: History -> Flags -> Flags
reactOnMidi h flags
    | midiIsActive h && midiDeviceIsEmpty flags = setMidiDevice flags
    | otherwise                                 = flags
    where
        midiIsActive = not . null . midis
        midiDeviceIsEmpty = isNothing . midiDevice . midiRT
        setMidiDevice x = x { midiRT = (midiRT x) { midiDevice = Just "a" } }


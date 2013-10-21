module Csound.Typed.Render(
    renderOut, renderOutBy, 
    renderEff, renderEffBy,
    renderOut_, renderOutBy_, 
    -- * Options
    module Csound.Typed.GlobalState.Options,
    module Csound.Dynamic.Flags
) where

import qualified Data.Map as M
import Data.Default
import Data.Maybe
import Data.Tuple

import Csound.Dynamic hiding (csdFlags)
import Csound.Dynamic.Control
import Csound.Dynamic.Flags
import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Options
import Csound.Typed.Control.Instr
import Csound.Typed.Control(getIns)

toCsd :: Tuple a => Options -> SE a -> IO Csd
toCsd options sigs = fmap (renderHistory (outArity sigs) options) 
    $ execGE options (saveMasterInstr (constArity sigs) (masterExp sigs))

renderOut_ :: SE () -> IO String
renderOut_ = renderOutBy_ def 

renderOutBy_ :: Options -> SE () -> IO String
renderOutBy_ options = fmap renderCsd . (toCsd options) . fmap (const unit)

renderOut :: Sigs a => SE a -> IO String
renderOut = renderOutBy def

renderOutBy :: Sigs a => Options -> SE a -> IO String
renderOutBy options = fmap renderCsd . (toCsd options)

renderEff :: (Sigs a, Sigs b) => (a -> SE b) -> IO String
renderEff = renderEffBy def

renderEffBy :: (Sigs a, Sigs b) => Options -> (a -> SE b) -> IO String
renderEffBy options eff = renderOutBy options $ eff =<< getIns

renderHistory :: Int -> Options -> History -> Csd
renderHistory nchnls opt hist = Csd flags orc sco
    where
        flags   = reactOnMidi hist $ csdFlags opt
        orc     = Orc (getInstr0 nchnls opt hist) (fmap (uncurry Instr) $ instrsContent $ instrs hist)
        sco     = Sco (Just $ getTotalDur opt $ totalDur hist) (renderGens $ genMap hist) [alwaysOn $ masterInstrId hist]

        renderGens = fmap swap . M.toList . idMapContent        

getInstr0 :: Int -> Options -> History -> InstrBody
getInstr0 nchnls opt hist = do
    globalConstants
    midiAssigns
    initGlobals
    renderBandLimited (genMap hist) (bandLimitedMap hist)
    userInstr0 hist
    chnUpdateUdo 
    where
        globalConstants = do
            setSr       $ defSampleRate opt
            setKsmps    $ defBlockSize opt
            setNchnls   (max 1 nchnls)
            setZeroDbfs 1

        midiAssigns = mapM_ renderMidiAssign $ midis hist

        initGlobals = varsInits $ globalsVars $ globals $ hist

reactOnMidi :: History -> Flags -> Flags
reactOnMidi h flags
    | midiIsActive h && midiDeviceIsEmpty flags = setMidiDevice flags
    | otherwise                                 = flags
    where
        midiIsActive = not . null . midis
        midiDeviceIsEmpty = isNothing . midiDevice . midiRT
        setMidiDevice x = x { midiRT = (midiRT x) { midiDevice = Just "a" } }


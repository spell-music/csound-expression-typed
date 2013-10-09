module Csound.Typed.Render(
    renderOut, renderOutBy,
    -- * Options
    module Csound.Typed.GlobalState.Options,
    module Csound.Dynamic.Flags
) where

import qualified Data.Map as M
import Data.Default
import Data.Maybe
import Data.Tuple

import Csound.Dynamic
import Csound.Dynamic.Control
import Csound.Dynamic.Flags
import Csound.Typed.Types(Out, outArity)
import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Options
import Csound.Typed.Control.Instr

toCsd :: Out a => Options -> a -> IO Csd
toCsd options sigs = fmap (renderHistory (outArity sigs) options) 
    $ execGE options (saveMasterInstr (masterArity sigs) (masterExp sigs))

renderOut :: Out a => a -> IO String
renderOut = renderOutBy def

renderOutBy :: Out a => Options -> a -> IO String
renderOutBy options = fmap renderCsd . (toCsd options)

renderHistory :: Int -> Options -> History -> Csd
renderHistory nchnls opt hist = Csd flags orc sco
    where
        flags   = reactOnMidi hist $ setFlags opt
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
            setSr       $ setSampleRate opt
            setKsmps    $ setBlockSize opt
            setNchnls   nchnls
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


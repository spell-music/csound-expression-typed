module Csound.Typed.Render(
    renderOut, renderOutBy,
    -- * Options
    module Csound.Typed.Types.GlobalState.Options
) where

import qualified Data.Map as M
import Data.Default
import Data.Tuple

import Csound.Dynamic
import Csound.Dynamic.Control
import Csound.Typed.Types(Out)
import Csound.Typed.Types.GlobalState
import Csound.Typed.Types.GlobalState.Options
import Csound.Typed.Control.Instr

toCsd :: Out a => Options -> a -> IO Csd
toCsd options sigs = fmap (renderHistory options) $ execGE options (saveMasterInstr (masterArity sigs) (masterExp sigs))

renderOut :: Out a => a -> IO String
renderOut = renderOutBy def

renderOutBy :: Out a => Options -> a -> IO String
renderOutBy options = fmap renderCsd . (toCsd options)

renderHistory :: Options -> History -> Csd
renderHistory opt hist = Csd flags orc sco
    where
        flags   = setFlags opt
        orc     = Orc (sysInstr0 hist >> userInstr0 hist) (fmap (uncurry Instr) $ instrsContent $ instrs hist)
        sco     = Sco (Just $ getTotalDur opt $ totalDur hist) (renderGens $ genMap hist) [alwaysOn $ masterInstrId hist]

        renderGens = fmap swap . M.toList . idMapContent        


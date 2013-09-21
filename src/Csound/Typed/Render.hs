module Csound.Typed.Render(
    renderOut
) where

import Csound.Dynamic
import Csound.Typed.Tuple(Out)

toCsd :: Out a => a -> IO Csd
toCsd = undefined

renderOut :: Out a => a -> IO String
renderOut = fmap renderCsd . toCsd


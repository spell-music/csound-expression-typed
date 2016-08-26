module Csound.Typed.Plugins(
    adsr140,
    audaciousEq,
    
    -- One pole filters
    zdf1, zlp1, zhp1, zap1,

    -- Two pole filters
    zdf2, zlp, zbp, zhp, zdf2_notch, zbr,

    -- Ladder filter
    zladder, 

    -- Four poles filters
    zdf4, zlp4, zbp4, zhp4, 

    -- Eq-filters
    peakEq, highShelf, lowShelf
) where

import Csound.Typed.Plugins.Adsr140
import Csound.Typed.Plugins.Zdf
import Csound.Typed.Plugins.Audaciouseq

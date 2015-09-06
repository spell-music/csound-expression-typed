module Csound.Typed.GlobalState.Options (
    Options(..),
    defGain, defSampleRate, defBlockSize, defTabFi,
    -- ** Table fidelity
    TabFi(..), fineFi, coarseFi,
    -- *** Gen identifiers
    -- | Low level Csound integer identifiers for tables. These names can be used in the function 'Csound.Base.fineFi'
    idWavs, idMp3s, idDoubles, idSines, idSines3, idSines2,
    idPartials, idSines4, idBuzzes, idConsts, idLins, idCubes,
    idExps, idSplines, idStartEnds,  idPolys, idChebs1, idChebs2, idBessels, idWins
) where

import Control.Applicative
import Data.Default
import qualified Data.IntMap as IM

import Csound.Dynamic hiding (csdFlags)

-- | Csound options. The default values are
--
-- > flags      = def     -- the only flag set by default is "no-displays" 
-- >                      -- to supress the display of the tables
-- > sampleRate = 44100
-- > blockSize  = 64
-- > gain       = 0.5
-- > tabFi      = fineFi 13 [(idLins, 11), (idExps, 11), (idConsts, 9), (idSplines, 11), (idStartEnds, 12)] }
data Options = Options 
    { csdFlags          :: Flags        -- ^ Csound command line flags
    , csdSampleRate     :: Maybe Int          -- ^ The sample rate
    , csdBlockSize      :: Maybe Int          -- ^ The number of audio samples in one control step
    , csdGain           :: Maybe Double       -- ^ A gain of the final output
    , csdTabFi          :: Maybe TabFi        -- ^ Default fidelity of the arrays
    }
   
instance Default Options where
    def = Options def def def def def

instance Monoid Options where
    mempty = def
    mappend a b = Options
        { csdFlags          = mappend (csdFlags a) (csdFlags b)
        , csdSampleRate     = csdSampleRate a <|> csdSampleRate b
        , csdBlockSize      = csdBlockSize a <|> csdBlockSize b
        , csdGain           = csdGain a <|> csdGain b
        , csdTabFi          = csdTabFi a <|> csdTabFi b }

defGain :: Options -> Double
defGain = maybe 0.8 id . csdGain

defSampleRate :: Options -> Int
defSampleRate = maybe 44100 id . csdSampleRate

defBlockSize :: Options -> Int
defBlockSize = maybe 64 id . csdBlockSize

defTabFi :: Options -> TabFi
defTabFi = maybe def id . csdTabFi
    
-- | Table size fidelity (how many points in the table by default).
data TabFi = TabFi
    { tabFiBase   :: Int
    , tabFiGens   :: IM.IntMap Int }

instance Default TabFi where
    def = fineFi 13 [(idLins, 11), (idExps, 11), (idConsts, 9), (idSplines, 11), (idStartEnds, 12)]
        

-- | Sets different table size for different GEN-routines. 
--
-- > fineFi n ps 
--
-- where 
-- 
-- * @n@ is the default value for table size (size is a @n@ power of 2) for all gen routines that are not listed in the next argument @ps@.
--
-- * @ps@ is a list of pairs @(genRoutineId, tableSizeDegreeOf2)@ that sets the given table size for a 
--   given GEN-routine.
--
-- with this function we can set lower table sizes for tables that are usually used in the envelopes.
fineFi :: Int -> [(Int, Int)] -> TabFi
fineFi n xs = TabFi n (IM.fromList xs)

-- | Sets the same table size for all tables. 
--
-- > coarseFi n
--
-- where @n@  is a degree of 2. For example, @n = 10@ sets size to 1024 points for all tables by default.
coarseFi :: Int -> TabFi
coarseFi n = TabFi n IM.empty

idWavs, idMp3s, idDoubles, idSines, idSines3, idSines2,
    idPartials, idSines4, idBuzzes, idConsts, idLins, idCubes,
    idExps, idSplines, idStartEnds,  idPolys, idChebs1, idChebs2, idBessels, idWins :: Int


-- Human readable Csound identifiers for GEN-routines

idWavs = 1
idDoubles = 2
idSines = 10
idSines3 = 9
idSines2 = 9
idPartials = 9
idSines4 = 19
idBuzzes = 11
idConsts = 17
idLins = 7
idCubes = 6
idExps = 5
idStartEnds = 16
idSplines = 8 
idPolys = 3
idChebs1 = 13
idChebs2 = 14
idBessels = 12
idWins = 20
idMp3s = 49


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-type-defaults -Wno-unused-top-binds #-}
-- | The JPL Horizons [1] on-line solar system data and ephemeris computation service provides access to key solar system data and flexible production of highly accurate ephemerides for solar system objects (1,180,796 asteroids, 3,789 comets, 211 planetary satellites {includes satellites of Earth and dwarf planet Pluto}, 8 planets, the Sun, L1, L2, select spacecraft, and system barycenters).
--
-- Horizons is provided by the Solar System Dynamics Group of the Jet Propulsion Laboratory.
--
-- 1) https://ssd.jpl.nasa.gov/horizons/
module API.JPL.Horizons (
  saveCsv,
  Body(..),
  -- -- * CSV export
  -- vecCsvBuilder, vecCsvHeader,
  -- bsbWriteFile
  ) where

import Control.Applicative (Alternative(..))
import Data.Functor (void)
import Data.Void
import Data.List (intercalate, intersperse)
import           System.IO (Handle, IOMode(..), withBinaryFile)
-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Builder as BSB (Builder, toLazyByteString, hPutBuilder, char8, string8)
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.ByteString.Char8 as BS8 (pack)
-- megaparsec
import qualified Text.Megaparsec as P (Parsec, ParseErrorBundle, try, parse, parseTest, some, satisfy, between, skipManyTill, takeWhileP)
import qualified Text.Megaparsec.Error as P (errorBundlePretty)
import qualified Text.Megaparsec.Byte as PL (space1)
import qualified Text.Megaparsec.Byte.Lexer as PL (space, lexeme, symbol, skipLineComment, skipBlockComment, scientific, float, signed)
-- req
import Network.HTTP.Req (runReq, defaultHttpConfig, req, GET(..), Option, Url, Scheme(..), https, (/:), NoReqBody(..), bsResponse, responseBody, (=:) )
import Data.Text (Text)
-- scientific
import Data.Scientific (Scientific)
import Data.ByteString.Builder.Scientific (scientificBuilder)
-- time
import Data.Time.Calendar (Day, toGregorian, fromGregorian)
import Data.Time.Clock (DiffTime)


-- | Make an API call, parse and save the results as CSV
--
-- The resulting file will contain one sample of the state vector per row
saveCsv :: Body -- ^ center body (observation site)
        -> (Day, Day) -- ^ (first, last) day of observation
        -> Int -- ^ observation interval in minutes
        -> Body -- ^ solar system body
        -> FilePath -- ^ CSV directory path
        -> IO ()
saveCsv centerb ds@(d0, d1) dt b fdir = do
  bsb <- get centerb ds dt b
  let
    fpath = fdir <> "/" <> mconcat (intersperse "_" [show b, time d0, time d1]) <> ".csv"
  bsbWriteFile fpath bsb

bsbWriteFile :: FilePath -> BSB.Builder -> IO ()
bsbWriteFile = modifyFile WriteMode
modifyFile :: IOMode -> FilePath -> BSB.Builder -> IO ()
modifyFile mode f bld = withBinaryFile f mode (`BSB.hPutBuilder` bld)

-- | Make an API call
get :: Body -> (Day, Day) -> Int -> Body -> IO BSB.Builder
get centerb ds dt b = do
  bs <- get0 $ opts centerb ds dt b
  case P.parse vectors "" bs of
    Right vs -> pure $
                  vecCsvHeader <>
                  foldMap vecCsvBuilder vs
    Left e -> error $ P.errorBundlePretty e

get0 :: Option 'Https -> IO BS.ByteString
get0 os = runReq defaultHttpConfig $ do
  r <- req GET endpoint NoReqBody bsResponse os
  pure $ responseBody r

opts :: Body -> (Day, Day) -> Int -> Body -> Option 'Https
opts cb (d0, d1) dt b =
  "format" ==: "text" <>
  "make_ephem" ==: "yes" <>
  "ephem_type" ==: "vectors" <>
  "center" ==: bodyToCommand cb <>
  "command" ==: bodyToCommand b <>
  "obj_data" ==: "no" <>
  "ref_system" ==: "icrf" <>
  "start_time" ==: time d0 <>
  "stop_time" ==: time d1 <>
  "step_size" ==: stepsizeMins dt

{-
    [id: 10]   Sun [Sol]
    [id:199]   Mercury
    [id:299]   Venus
    [id:399]   Earth
    [id:301]   Moon
    [id:499]   Mars
    [id:599]   Jupiter
    [id:699]   Saturn
    [id:799]   Uranus
    [id:899]   Neptune
-}

-- | Large bodies in the Solar System
data Body = Sun
          | Mercury
          | Venus
          | Earth | Moon
          | Mars
          | Jupiter | Io | Europa | Ganymede | Callisto | Amalthea
          | Saturn | Mimas | Enceladus | Tethys | Dione | Rhea | Titan | Hyperion | Iapetus | Phoebe | Janus | Epimetheus
          | Uranus
          | Neptune | Triton
          | Pluto
          | Eris
          deriving (Eq, Show, Enum)

class IsBody c where
  bodyToCommand :: c -> String
instance IsBody Body where
  bodyToCommand = \case
    Sun -> "10"
    Mercury -> "199"
    Venus -> "299"
    Earth -> "399"
    Moon -> "301"
    Mars -> "499"
    Jupiter -> "599"
    Io -> "501"
    Europa -> "502"
    Ganymede -> "503"
    Callisto -> "504"
    Amalthea -> "505"
    Saturn -> "699"
    Mimas -> "601"
    Enceladus -> "602"
    Tethys -> "603"
    Dione -> "604"
    Rhea -> "605"
    Titan -> "606"
    Hyperion -> "607"
    Iapetus -> "608"
    Phoebe -> "609"
    Janus -> "610"
    Epimetheus -> "611"
    Uranus -> "799"
    Neptune -> "899"
    Triton -> "801"
    Pluto -> "999"
    Eris -> "136199"

stepsizeMins :: Int -> String
stepsizeMins m = show m <> "m"

time :: Day -> String
time d = intercalate "-" [show yy, show mm, show dd]
  where
    (yy, mm, dd) = toGregorian d

(==:) :: Text -> String -> Option 'Https
(==:) = (=:)

endpoint :: Url 'Https
endpoint = https "ssd.jpl.nasa.gov" /: "api" /: "horizons.api"

{-
2453736.500000000 = A.D. 2006-Jan-01 00:00:00.0000 TDB 
 X = 8.749529331045696E+07 Y = 7.604048145779434E+07 Z = 3.126488404274795E+06
 VX= 7.367178825395701E+00 VY= 1.398625134891928E+01 VZ= 7.299094429880579E-01
 LT= 3.868100505438247E+02 RG= 1.159627358316374E+08 RR= 1.474953828661886E+01
-}


vectors :: Parser [Vec]
vectors = P.some header *> payload (P.some vec)

data Vec = Vec Scientific Scientific Scientific Scientific Scientific Scientific deriving (Show)

-- | CSV Header
vecCsvHeader :: BSB.Builder
vecCsvHeader = csvBuild BSB.string8 ["X", "Y", "Z", "VX", "VY", "VZ"]
-- | CSV data row
vecCsvBuilder :: Vec -> BSB.Builder
vecCsvBuilder (Vec v0x v0y v0z vvx vvy vvz) =
  csvBuild scientificBuilder [v0x, v0y, v0z, vvx, vvy, vvz]

csvBuild :: (t -> BSB.Builder) -> [t] -> BSB.Builder
csvBuild _ [] = mempty
csvBuild bfun (w:ws) = bfun w <> go ws
  where
    go (m:ms) = BSB.string8 "," <> bfun m <> go ms
    go [] = BSB.string8 "\n"


-- | timestamp line e.g.
--
-- 2453736.500000000 = A.D. 2006-Jan-01 00:00:00.0000 TDB
timestamp :: Parser ()
timestamp = PL.float *> PL.space1 *> skipLine "= A.D."

vec :: Parser Vec
vec = do
  timestamp
  cx <- x <* PL.space1
  cy <- y <* PL.space1
  cz <- z <* PL.space1
  cvx <- vx <* PL.space1
  cvy <- vy <* PL.space1
  cvz <- vz <* PL.space1
  _ <- lt <* PL.space1
  _ <- rg <* PL.space1
  _ <- rr <* PL.space1
  pure $ Vec cx cy cz cvx cvy cvz

x, y, z, vx, vy, vz, lt, rg, rr :: Parser Scientific
x = vcomp "X"
y = vcomp "Y"
z = vcomp "Z"
vx = vcomp "VX"
vy = vcomp "VY"
vz = vcomp "VZ"
lt = vcomp "LT"
rg = vcomp "RG"
rr = vcomp "RR"

vcomp :: String -> Parser Scientific
vcomp vv = psymbol (BS8.pack vv) >> psymbol "=" >> scientific

scientific :: Parser Scientific
scientific = PL.signed space PL.scientific

payload :: Parser a -> Parser a
payload = P.between (psymbol "$$SOE") (psymbol "$$EOE")

-- payloadDelim :: Parser ()
-- payloadDelim = void $ psymbol "$$SOE"

header :: Parser ()
header = skipLine "Ephemeris" <|>
         skipLine "API" <|>
         skipLine "Target" <|>
         skipLine "Center" <|>
         skipLine "Output" <|>
         skipLine "EOP" <|>
         skipLine "Start" <|> skipLine "Stop" <|> skipLine "Step" <|> skipLine "Reference" <|> skipLine "JDTDB" <|>
         skipLine "X" <|> skipLine "Y" <|> skipLine "Z" <|>
         skipLine "VX" <|> skipLine "VY" <|> skipLine "VZ" <|>
         skipLine "LT" <|> skipLine "RG" <|> skipLine "RR"

skipLine :: String -> Parser ()
skipLine s = psymbol (BS8.pack s) *>
             void (P.takeWhileP (Just "") (\c -> c /= BS.c2w '\n')) *>
             void (psymbol ("\n"))

type Parser = P.Parsec Void BS.ByteString
type ParseErrorBundle = P.ParseErrorBundle BS.ByteString Void


psymbol :: BS.ByteString -> Parser BS.ByteString
psymbol = PL.symbol space

space :: Parser ()
space = PL.space PL.space1 lineComment blockComment

lineComment, blockComment :: Parser ()
lineComment = PL.skipLineComment "****"

blockComment = PL.skipBlockComment "/**" "*/"

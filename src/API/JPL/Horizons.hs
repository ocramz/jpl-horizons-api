{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module API.JPL.Horizons where
import Data.Void
import Data.List (intercalate)
-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)
-- megaparsec
import qualified Text.Megaparsec as P (Parsec, ParseErrorBundle, try, parse, parseTest, some, satisfy, between)
import qualified Text.Megaparsec.Byte as PL (space1)
import qualified Text.Megaparsec.Byte.Lexer as PL (space, lexeme, symbol, skipLineComment, skipBlockComment, scientific)
-- req
import Network.HTTP.Req (runReq, defaultHttpConfig, req, GET(..), Option, Url, Scheme(..), https, (/:), NoReqBody(..), bsResponse, responseBody, (=:) )
import Data.Text (Text)
-- scientific
import Data.Scientific (Scientific)
-- time
import Data.Time.Calendar (Day, toGregorian, fromGregorian)

get :: (Day, Day) -> Int -> IO (Either ParseErrorBundle [Vec])
get ds dt = do
  bs <- get0 $ opts ds dt
  pure $ P.parse vectors "" bs

get0 :: Option 'Https -> IO BS.ByteString
get0 os = runReq defaultHttpConfig $ do
  r <- req GET endpoint NoReqBody bsResponse os
  pure $ responseBody r

-- opts :: Option 'Https
opts :: (Day, Day) -> Int -> Option 'Https
opts (d0, d1) dt =
  "format" ==: "text" <>
  "make_ephem" ==: "yes" <>
  "ephem_type" ==: "vector" <>
  "command" ==: "499" <> -- Mars
  "obj_data" ==: "no" <>
  "ref_system" ==: "icrf" <>
  "start_time" ==: time d0 <>
  "end_time" ==: time d1 <>
  "step_size" ==: stepsizeMins dt

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
vectors = P.some v

data Vec = Vec Scientific Scientific Scientific Scientific Scientific Scientific deriving (Show)

v :: Parser Vec -- (Scientific, Scientific, Scientific, Scientific, Scientific, Scientific)
v = do
  cx <- x <* PL.space1
  cy <- y <* PL.space1
  cz <- z <* PL.space1
  cvx <- vx <* PL.space1
  cvy <- vy <* PL.space1
  cvz <- vz <* PL.space1
  pure $ Vec cx cy cz cvx cvy cvz

x, y, z, vx, vy, vz :: Parser Scientific
x = vcomp "X"
y = vcomp "Y"
z = vcomp "Z"
vx = vcomp "VX"
vy = vcomp "VY"
vz = vcomp "VZ"

vcomp :: String -> Parser Scientific
vcomp vv = psymbol (BS8.pack vv) >> psymbol "=" >> PL.scientific

payload :: Parser a -> Parser a
payload = P.between s s where
  s = psymbol "$$SOE"

type Parser = P.Parsec Void BS.ByteString
type ParseErrorBundle = P.ParseErrorBundle BS.ByteString Void


psymbol :: BS.ByteString -> Parser BS.ByteString
psymbol = PL.symbol space

space :: Parser ()
space = PL.space PL.space1 lineComment blockComment

lineComment, blockComment :: Parser ()
lineComment = PL.skipLineComment "%*"

blockComment = PL.skipBlockComment "/**" "*/"

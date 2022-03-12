{-# OPTIONS_GHC -Wno-unused-imports -Wno-type-defaults -Wno-unused-top-binds #-}
module Main where

import Control.Applicative (Alternative(..))
-- optparse
import Options.Applicative (Parser, ParserInfo, customExecParser, prefs, showHelpOnEmpty, showHelpOnError, execParser, (<**>), helper, info, fullDesc, progDesc, header, strOption, option, auto, long, short, flag', value, showDefault, metavar)
-- time
import Data.Time.Calendar (Day, toGregorian, fromGregorian)

import API.JPL.Horizons (saveCsv, Body(..))



main :: IO ()
main = do
  (Cfg cbody d0 d1 r bdy) <- customExecParser (prefs showHelpOnError) opts
  saveCsv cbody (d0, d1) r bdy "data"


opts :: ParserInfo Cfg
opts = info (cfgP <**> helper)
  ( fullDesc
  <> progDesc "jpl-horizons-api"
  <> header "jpl-horizons-api - download ephemerides in CSV format" )

data Cfg = Cfg {
  cfgCenterBody :: Body
  , cfgDay0 :: Day
  , cfgDay1 :: Day
  , cfgResolnMin :: Int
  , cfgBody :: Body 
               }

cfgP :: Parser Cfg
cfgP = Cfg <$>
  centerBodyP <*>
       day0P <*>
       day1P <*>
       resolnP <*>
       bodyP

resolnP :: Parser Int
resolnP = option auto (long "resolution" <>
                       metavar "MINUTES" <>
                      value 30 <>
                      showDefault)

day0P :: Parser Day
day0P = fromGregorian <$>
          option auto (long "year_start" <> metavar "YEAR") <*>
          option auto (long "month_start" <> value 1 <> showDefault) <*>
          option auto (long "day_start" <> value 1 <> showDefault)

day1P :: Parser Day
day1P = fromGregorian <$>
          option auto (long "year_stop" <> metavar "YEAR") <*>
          option auto (long "month_stop" <> value 1 <> showDefault) <*>
          option auto (long "day_stop" <> value 1 <> showDefault)


centerBodyP :: Parser Body
centerBodyP = flag' Sun (long "sun") <|>
              flag' Earth (long "earth")

bodyP :: Parser Body
bodyP = flag' Sun (long "sun") <|>
        flag' Mercury (long "mercury") <|>
        flag' Venus (long "venus") <|>
        flag' Earth (long "earth") <|>
        flag' Moon (long "moon") <|>
        flag' Mars (long "mars") <|>
        flag' Jupiter (long "jupiter") <|>
        flag' Saturn (long "saturn") <|>
        flag' Uranus (long "uranus") <|>
        flag' Pluto (long "pluto")

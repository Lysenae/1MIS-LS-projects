module Lib.Config.CmdArgs where

import Options.Applicative
import Data.Semigroup ((<>))

data CmdArgs = CmdArgs
  { hello  :: String
  , quiet  :: Bool
  , repeat :: Int }

config :: Parser CmdArgs
config = CmdArgs
  <$> strOption
      ( long "hello"
     <> metavar "TARGET"
     <> help "Target for the greeting" )
  <*> switch
      ( long "quiet"
     <> short 'q'
     <> help "Whether to be quiet" )
  <*> option auto
      ( long "repeat"
     <> help "Repeats for greeting"
     <> showDefault
     <> value 1
     <> metavar "INT" )

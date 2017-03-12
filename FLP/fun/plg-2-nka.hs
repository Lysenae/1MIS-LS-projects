module Main
    (main)
  where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad

import Lib.Config.CmdArgs

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: CmdArgs -> IO ()
greet (CmdArgs h False n) = replicateM_ n . putStrLn $ "Hello, " ++ h
greet _ = return ()

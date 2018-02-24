module Main where

import qualified Data.Text as T
import           Dhall
import           Fit
import           Protolude

main :: IO ()
main = do
  args <- getArgs
  cfg <- input auto "./data/config.dhall"
  putText $ show cfg
  let cmd = head $ T.pack <$> args
  display (cfg :: FitConfig) cmd

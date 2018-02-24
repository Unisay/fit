module Main where

import Protolude
import Fit.UI
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  display $ head $ T.pack <$> args

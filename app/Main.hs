module Main where

import qualified Junit


main :: IO ()
main = do
  res <- Junit.parseFile ""
  print res
  pure ()

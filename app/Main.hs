module Main where

import Brick ((<+>))
import qualified Brick as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import Junit (parseFile)
import System.Environment (getArgs)


ui :: B.Widget ()
ui = B.withBorderStyle B.unicode $
  B.borderWithLabel (B.str "Junit Test Results")
  (B.center (B.str "left") <+> B.center (B.str "right"))


main :: IO ()
main = do
  [arg] <- getArgs
  res <- Junit.parseFile arg
  B.simpleMain ui

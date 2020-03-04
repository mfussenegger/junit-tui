{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import Brick ((<+>))
import qualified Brick as B
import qualified Brick.AttrMap as B
import qualified Brick.Types as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.List as B
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty
import qualified Graphics.Vty.Input.Events as Vty
import qualified Junit
import System.Environment (getArgs)



data Env
  = Env
      { testSuites :: TestSuites,
        selectedSuite :: Int
      }
  deriving (Show)


data Name
  = TestSuiteList
  | TestCaseList
  deriving (Eq, Ord, Show)


type Event = ()
type TestSuites = B.List Name Junit.TestSuite


appDraw :: Env -> [B.Widget Name]
appDraw env = [renderedTestSuite]
  where
    renderedTestSuite =
      B.renderList renderTestSuite True testSuites' <+> B.renderList renderTestCase True testCases
    renderTestSuite hasFocus e = B.str . T.unpack $ Junit.name e
    renderTestCase hasFocus e = B.str . T.unpack $ Junit.tcName e
    testSuites' = testSuites env
    testCases = B.list TestCaseList testCases' 1
      where
        testCases' = case B.listSelectedElement testSuites' of
          Nothing -> V.empty
          Just (idx, testSuite) -> V.fromList $ Junit.testcases testSuite


appHandleEvent :: Env -> B.BrickEvent Name Event -> B.EventM Name (B.Next Env)
appHandleEvent env (B.VtyEvent (Vty.EvKey Vty.KEsc [])) = B.halt env
appHandleEvent env (B.VtyEvent e) = do
  newList <- B.handleListEvent e (testSuites env)
  B.continue $ env { testSuites = newList }
appHandleEvent env _ = B.continue env


appAttrMap :: Env -> B.AttrMap
appAttrMap _ = B.attrMap Graphics.Vty.defAttr []


rightOrFail :: (Show a) => Either a b -> IO b
rightOrFail (Left err) = error $ show err
rightOrFail (Right result) = pure result


loadEnv :: FilePath -> IO Env
loadEnv arg = do
  eTestSuite <- Junit.parseFile arg >>= rightOrFail
  pure $
    Env
      { testSuites = B.list TestSuiteList (V.fromList [eTestSuite]) 1,
        selectedSuite = 0
      }



main :: IO ()
main = do
  [arg] <- getArgs
  env <- loadEnv arg
  let app =
        B.App
          { B.appDraw = appDraw,
            B.appChooseCursor = B.neverShowCursor,
            B.appHandleEvent = appHandleEvent,
            B.appStartEvent = pure,
            B.appAttrMap = appAttrMap
          }
  finalState <- B.defaultMain app env
  pure ()

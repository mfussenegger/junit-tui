
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Junit (
  parseFile
) where


import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Read as T
import qualified Text.XML as XML


data TestSuite
  = TestSuite
      { name :: Text,
        tests :: Int,
        skipped :: Int,
        failures :: Int,
        errors :: Int,
        time :: Double,
        testcases :: [TestCase]
      }
  deriving (Eq, Show)

data TestCase
  = TestCase
      { name :: Text,
        classname :: Text,
        time :: Double,
        failure :: Maybe Failure
      }
  deriving (Eq, Show)


data Failure
  = Failure
      { message :: Text,
        typeName :: Text
      }
  deriving (Eq, Show)


unnestEither :: Monoid a => [Either a b] -> Either a [b]
unnestEither xs =
  if null lefts
    then Right rights
    else Left $ mconcat lefts
  where
    (lefts, rights) = partitionEithers xs


parseTestCases :: [XML.Node] -> Either String [TestCase]
parseTestCases nodes = unnestEither $ fmap parseTestCase testCaseNodes
  where
    testCaseNodes = mapMaybe testCaseNode nodes
    testCaseNode (XML.NodeElement element) =
      if XML.elementName element == "testcase" then Just element else Nothing
    testCaseNode _ = Nothing


eLookup :: Ord a => M.Map a b -> a -> String -> Either String b
eLookup map key errMsg =
  case M.lookup key map of
    Nothing -> Left errMsg
    Just val -> Right val


parseFailure :: [XML.Node] -> Either String (Maybe Failure)
parseFailure nodes =
  case failureElements of
    [] -> Right Nothing
    [failureElement] -> parseFailure' failureElement
    _ -> Left "More than 1 failure found. This is not expected"
  where
    failureElements = mapMaybe maybeFailureElement nodes
    maybeFailureElement (XML.NodeElement element) =
      if XML.elementName element == "failure" then Just element else Nothing
    maybeFailureElement _ = Nothing
    parseFailure' element = do
      message <- lookup "message" "message not found in failure element"
      typeName <- lookup "type" "type not found in failure element"
      pure $ Just Failure { message, typeName }
      where
        lookup = eLookup (XML.elementAttributes element)


parseTestCase :: XML.Element -> Either String TestCase
parseTestCase element = do
  classname <- lookup "classname" "classname not found in testcase"
  name <- lookup "name" "name not found in testcase"
  failure <- parseFailure $ XML.elementNodes element
  pure TestCase { name, classname, time, failure }
  where
    time = 0.0
    lookup = eLookup (XML.elementAttributes element)


parseDoc :: XML.Document -> Either String TestSuite
parseDoc doc = do
  name <- lookup "name" "Name for testsuite not found"
  tests <- fst <$> (T.decimal =<< lookup "tests" "Number of tests not found")
  skipped <- fst <$> (T.decimal =<< lookup "skipped" "Number of skipped tests not found")
  failures <- fst <$> (T.decimal =<< lookup "failures" "Number of failed tests not found")
  errors <- fst <$> (T.decimal =<< lookup "errors" "Number of errors not found")
  testcases <- parseTestCases $ XML.elementNodes testSuiteElement
  pure $ TestSuite { name, tests, skipped, failures, errors, time, testcases }
  where
    time = 0.0
    testSuiteElement = XML.documentRoot doc
    lookup = eLookup (XML.elementAttributes testSuiteElement)


parseFile :: FilePath -> IO (Either String TestSuite)
parseFile filepath = parseDoc <$> XML.readFile XML.def filepath

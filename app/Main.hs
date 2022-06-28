{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import System.Environment (getArgs)

import Data.Char (isDigit)
import Data.List (isSubsequenceOf, stripPrefix)
import Data.Maybe
import qualified Data.HashMap.Strict as Map

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty ( renderHtml )
import Text.Blaze.Html4.FrameSet (body, col)

import Debug.Trace
import Data.HashMap.Strict (foldMapWithKey)
import GHC.Base (Semigroup)

data TestResult = Pass | Fail
  deriving (Show)

data Test = Test
  { testName :: String,
    testCollection :: String,
    testResult :: Maybe TestResult,
    testTime :: Maybe Int
  } deriving (Show)

instance H.ToMarkup Test where
  toMarkup (Test n c (Just r) t) = H.tr ! resultColor r $ do
    H.td $ H.toHtml n
    H.td . H.toHtml $ show r
    H.td . H.toHtml $ case t of
                        Nothing -> "Timeout"
                        Just t -> show t ++ "s"
    H.td $ H.a ! A.href (H.toValue $ c ++ "." ++ n) $ "log"
    where
      resultColor Pass = A.class_ "table-success"
      resultColor Fail = A.class_ "table-danger"

newtype TestCollections = TestCollections
  { toMap :: Map.HashMap String [Test]
  }

--instance Functor TestCollections where
--  fmap f = Map.map f . toMap
instance Semigroup TestCollections where
 a <> b = TestCollections $ Map.unionWith (<>) (toMap a) (toMap b)

instance Monoid TestCollections where
  mempty = TestCollections Map.empty
  mappend = (<>)

  -- | XfsTests probably need to treat xfstests speically.

testMaybeInsert :: String -> Maybe Test -> TestCollections -> TestCollections
testMaybeInsert _ Nothing = id
testMaybeInsert col test = TestCollections . Map.alter alterDo col . toMap
  where
    alterDo Nothing   = fmap (:[]) test
    alterDo _ = error "This shouldn't happen"

fileToTest fs = Test a b Nothing Nothing
  where (b, a) = splitExtension fs


parseTests :: (String, String) -> [String] -> TestCollections
parseTests (col, _:name) = foldr parseTest mempty
  where
    parseTest = testMaybeInsert col . go ( Test name col Nothing Nothing )
    go _ "" = Nothing
    go t cs@(c:s)
      | isDigit c || c == ' ' || c == '='           = go t s
      | c == 'P' && "PASSED " `isSubsequenceOf` cs  =  Just $ parseNT (t { testResult = Just Pass }) (drop 6 s)
      | c == 'F' && "FAILED " `isSubsequenceOf` cs  =  Just $ parseNT (t { testResult = Just Fail }) (drop 6 s)
      | c == 'T' && "TEST FAILED " `isSubsequenceOf` cs  = Just (t { testResult = Just Fail
                                                              })
      | otherwise =  Nothing
    parseNT t s = t2
        where
          (t1, s1) = parseName t s
          (t2, s2) = parseTime t1 s1
    parseTime t s = let ms = stripPrefix " in " s in ((t { testTime = fmap (read . takeWhile isDigit) ms }), ms)
    parseName t s = (t, dropWhile (/= ' ') s)


testsToHtml :: String -> TestCollections -> H.Html
testsToHtml rev tests = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml ("Tests for rev: " ++ rev)
    H.link  ! A.href "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css"
            ! A.rel "stylesheet"
  H.body $ do
    H.div ! A.class_ "container" $ do
      H.h1 $ H.toHtml ("Rev: " ++ rev)
      Map.foldMapWithKey (\name subtests -> do
                            H.h2 (H.toHtml name)
                            H.table ! A.class_ "table" $ foldMap H.toHtml subtests) $ toMap tests


main :: IO ()
main = do
  args <- getArgs
  if length args < 2 then
    hPutStrLn stderr "help: <git-rev> <test directory>"
    else do
      let rev:directory:_ = args
      fs <- listDirectory directory
      tests <- flip foldMap ( filter (\a -> elem '.' a || "index.html" == a) fs) $ \fileName -> do
        hPutStrLn stderr fileName
        fc <- readFile $ directory </> fileName
        return (parseTests (splitExtension fileName) . lines $ fc)
      putStr . renderHtml . testsToHtml rev $ tests

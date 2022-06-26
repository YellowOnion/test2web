{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import Data.Char (isDigit)
import Data.List (isSubsequenceOf, stripPrefix)
import Data.Maybe
import Debug.Trace
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty ( renderHtml )
import Text.Blaze.Html4.FrameSet (body)


data TestResult = Pass | Fail
  deriving (Show)

data Test = Test
  { testName :: String,
    testCollection :: String,
    testResult :: Maybe TestResult,
    testTime :: Maybe Int
  } deriving (Show)

instance H.ToMarkup Test where
  toMarkup (Test n _ r t) = H.tr $ do
    H.td $ H.toHtml n
    H.td . H.toHtml $ case r of
                       Nothing -> "err"
                       Just r -> show r
    H.td . H.toHtml $ case t of
                        Nothing -> "Timeout"
                        Just t -> show t ++ "s"

fileToTest fs = Test a b Nothing Nothing
  where (b, a) = splitExtension fs

directory = "out"

parseTests :: [String] -> [Maybe Test]
parseTests = map parseTest
  where
    parseTest = go ( Test "" "" Nothing Nothing )
    go _ "" = Nothing
    go t cs@(c:s)
      | isDigit c || c == ' ' || c == '='           = go t s
      | c == 'P' && "PASSED " `isSubsequenceOf` cs  =  Just $ parseNT (t { testResult = Just Pass }) (drop 6 s)
      | c == 'F' && "FAILED " `isSubsequenceOf` cs  =  Just $ parseNT (t { testResult = Just Fail }) (drop 6 s)
      | otherwise =  Nothing
    parseNT t s = t2
        where
          (t1, s1) = parseName t s
          (t2, s2) = parseTime t1 s1
    parseTime t s = let ms = stripPrefix " in " s in ((t { testTime = fmap (read . takeWhile isDigit) ms }), ms)
    parseName t s = ((t { testName = takeWhile (/= ' ') s }), dropWhile (/= ' ') s)
    parseName t s = trace (show ((show t), s)) error "blah"


testsToHtml :: String -> [(String, [Test])] -> H.Html
testsToHtml rev tests = H.docTypeHtml $ do
  H.head $ H.title $ H.toHtml ("Tests for rev:" ++ rev)
  H.body $ do
    H.h1 $ H.toHtml ("Rev: " ++ rev)
    foldMap (\(name, subtests) -> do
                          H.h2 (H.toHtml name)
                          H.table $ foldMap H.toHtml subtests) tests


main :: IO ()
main = do
  fs <- listDirectory directory
  tests <- forM ( filter (notElem '.') fs) $ \fileName -> do
    fc <- readFile $ directory </> fileName
    return (fileName, catMaybes . parseTests . lines $ fc)
  writeFile "output.html". renderHtml . testsToHtml "0x00000" $ tests

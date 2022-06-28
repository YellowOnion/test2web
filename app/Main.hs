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

import qualified Debug.Trace as Trace
import Data.HashMap.Strict (foldMapWithKey)
import GHC.Base (Semigroup)


trace' a = Trace.trace (show a) a

data TestResult = Pass | Fail
  deriving (Show)

data Test = Test
  { testName :: String,
    testResult :: Maybe TestResult,
    testTime :: Maybe Int
  } deriving (Show)

instance H.ToMarkup Test where
  toMarkup (Test n (Just r) t) = H.tr ! resultColor r $ do
    H.td $ H.toHtml n
    H.td . H.toHtml $ show r
    H.td . H.toHtml $ case t of
                        Nothing -> "Timeout"
                        Just t -> show t ++ "s"
    H.td $ H.a ! A.href (H.toValue n) $ "log"
    where
      resultColor Pass = A.class_ "table-success"
      resultColor Fail = A.class_ "table-danger"


parseTests :: String -> [String] -> [Test]
parseTests name = mapMaybe parseTest
  where
    parseTest = go ( Test name Nothing Nothing )
    go _ "" = Nothing
    go t cs@(c:s)
      | isDigit c || c == ' ' || c == '='           = go t s
      | c == 'P' && "PASSED " `isSubsequenceOf` cs  =  Just $ parseNT (t { testResult = Just Pass }) (drop 6 s)
      | c == 'F' && "FAILED " `isSubsequenceOf` cs  =  Just $ parseNT (t { testResult = Just Fail }) (drop 6 s)
      | c == 'T' && "TEST FAILED " `isSubsequenceOf` cs  = Just (t { testResult = Just Fail
                                                              })
      | otherwise = error name
    parseNT t s = t2
        where
          (t1, s1) = parseName t s
          (t2, s2) = parseTime t1 s1
    parseTime t s = let ms = stripPrefix " in " s in ((t { testTime = fmap (read . takeWhile isDigit) ms }), ms)
    parseName t s = (t, dropWhile (/= ' ') s)


testsToHtml :: String -> [Test] -> H.Html
testsToHtml rev tests = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml ("Tests for rev: " ++ rev)
    H.link  ! A.href "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css"
            ! A.rel "stylesheet"
  H.body $ do
    H.div ! A.class_ "container" $ do
      H.h1 $ H.toHtml ("Rev: " ++ rev)
      (H.table ! A.class_ "table") $ foldMap H.toHtml tests

readLastLine :: Handle -> IO String
readLastLine h =  hSeek h SeekFromEnd 0 >> go False ""
  where go a s = do
          n <- hTell h
          if n <= 0 then return s
            else do
              hSeek h RelativeSeek (-1)
              c <- hGetChar h
              hSeek h RelativeSeek (-1)
              if not ( c == '\n' && a)
                then go (c /= '\n') (c:s)
                else return s


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
        fc <- withFile (directory </> fileName) ReadMode readLastLine
        return (parseTests fileName . lines $ fc)
      putStr . renderHtml . testsToHtml rev $ tests

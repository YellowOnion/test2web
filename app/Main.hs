{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Char (isDigit)
import qualified Data.HashMap.Strict as Map
import Data.List (isSubsequenceOf, stripPrefix)
import Data.Maybe
import qualified Debug.Trace as Trace
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Test

trace' a = Trace.trace (show a) a

instance H.ToMarkup Test where
  toMarkup (Test n (Just r) t) = H.tr ! resultColor r $ do
    H.td $ H.toHtml n
    H.td . H.toHtml $ show r
    H.td . H.toHtml $ case t of
      Nothing -> "Timeout"
      Just t -> show t ++ "s"
    H.td $ H.a ! A.href (H.toValue n) $ "log"
    H.td $ H.a ! A.href (H.toValue (takeWhile (/='.') n)) $ "full log"
    where
      resultColor Pass = A.class_ "table-success"     -- Green
      resultColor Fail = A.class_ "table-danger"      -- Red
      resultColor Unknown = A.class_ "table-secondary" -- Grey

parseTests :: String -> [String] -> [Test]
parseTests name = mapMaybe parseTest
  where
    parseTest = go (Test name Nothing Nothing)
    go t "" = Just $ t & result ?~ Unknown
    go t cs@(c : s)
      | isDigit c || c == ' ' || c == '=' = go t s
      | c == 'P' && "PASSED " `isSubsequenceOf` cs = Just $ parseNT (t & result ?~ Pass) (drop 6 s)
      | c == 'F' && "FAILED " `isSubsequenceOf` cs = Just $ parseNT (t & result ?~ Fail) (drop 6 s)
      | c == 'T' && "TEST FAILED " `isSubsequenceOf` cs = Just $ t & result ?~ Fail
      | otherwise = Just $ t & result ?~ Unknown
    parseNT t s = t2
      where
        (t1, s1) = parseName t s
        (t2, s2) = parseTime t1 s1
    parseTime t s = let ms = stripPrefix " in " s in (t & time .~ fmap (read . takeWhile isDigit) ms, ms)
    parseName t s = (t, dropWhile (/= ' ') s)

testsToHtml :: String -> [Test] -> H.Html
testsToHtml rev tests = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml ("Tests for rev: " ++ rev)
    H.link ! A.href "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css"
      ! A.rel "stylesheet"
  H.body $ do
    H.div ! A.class_ "container" $ do
      H.h1 $ H.toHtml ("Rev: " ++ rev)
      (H.table ! A.class_ "table") $ foldMap H.toHtml tests

readLastLine :: Handle -> IO String
readLastLine h = hSeek h SeekFromEnd 0 >> go False ""
  where
    go a s = do
      n <- hTell h
      if n <= 0
        then return s
        else do
          hSeek h RelativeSeek (-1)
          c <- hGetChar h
          hSeek h RelativeSeek (-1)
          if not (c == '\n' && a)
            then go (c /= '\n') (c : s)
            else return s

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then hPutStrLn stderr "help: <git-rev> <test directory>"
    else do
      let rev : directory : _ = args
      fs <- listDirectory directory
      tests <- flip foldMap (filter (\a -> elem '.' a || "index.html" == a) fs) $ \fileName -> do
        hPutStrLn stderr fileName
        fc <- withFile (directory </> fileName) ReadMode readLastLine
        return (parseTests fileName . lines $ fc)
      putStr . renderHtml . testsToHtml rev $ tests

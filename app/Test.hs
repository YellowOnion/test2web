{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Lens

data TestResult = Pass | Fail | Unknown
  deriving (Show)

data Test = Test
  { _name :: String,
    _result :: Maybe TestResult,
    _time :: Maybe Int
  }
  deriving (Show)

makeLenses ''Test


{-# LANGUAGE DeriveGeneric #-}

module Config where

import GHC.Generics


data Config = Config
  { templateDir :: FilePath
  , resultDir   :: FilePath
  , renderCmd   :: String
  }
  deriving Generic

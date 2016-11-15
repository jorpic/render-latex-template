
{-# LANGUAGE DeriveGeneric #-}

module Config where

import GHC.Generics


data Config = Config
  { templateDir :: FilePath
  , workDir     :: FilePath
  , docDir      :: FilePath
  , renderCmd   :: String
  , httpPort    :: Int
  }
  deriving Generic


{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad (void)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Configurator as Config

import           System.IO (hPutStrLn, stderr)
import qualified System.Environment as Env
import qualified System.Process as P

import GHC.Generics


data Config = Config
  { templateDir :: FilePath
  , resultDir   :: FilePath
  , renderCmd   :: String
  }
  deriving Generic


main :: IO ()
main = do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    [configPath] -> do
      cfg <- Config.load [Config.Required configPath]
      config <- Config
        <$> Config.require cfg "template_dir"
        <*> Config.require cfg "result_dir"
        <*> Config.require cfg "render_cmd"
      main' config
    _ -> hPutStrLn stderr $ "Usage: " ++ prog ++ " <config.conf>"


main' :: Config -> IO ()
main' (Config {..}) = do
  (code, sOut, sErr) <- P.readCreateProcessWithExitCode
    (P.shell renderCmd) {P.cwd = Just templateDir}
    ""

--  (_, hOut, _, hProc) <- P.createProcess (P.shell renderCmd)
--    { P.cwd = Just templateDir
--    , P.std_out = P.CreatePipe
--    }
--  void $ P.waitForProcess hProc

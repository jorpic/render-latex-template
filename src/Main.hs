

module Main where

import qualified Data.Configurator as Config
import qualified System.Environment as Env

import Config
import Job (initWorkspace)
import Server (runServer)
import Utils (report)



main :: IO ()
main = do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    [configPath] -> do
      report "main: reading config"
      cfg <- Config.load [Config.Required configPath]
      config <- Config
        <$> Config.require cfg "template_dir"
        <*> Config.require cfg "work_dir"
        <*> Config.require cfg "doc_dir"
        <*> Config.require cfg "render_cmd"
        <*> Config.require cfg "http_port"

      initWorkspace config
      runServer config
    _ -> report $ "Usage: " ++ prog ++ " <config.conf>"

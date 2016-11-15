

module Main where

import           Control.Monad (guard)
import qualified Data.Aeson as Aeson
import qualified Data.Configurator as Config
import qualified System.Environment as Env

import Config
import Render
import Utils



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
    _ -> report $ "Usage: " ++ prog ++ " <config.conf>"


main' :: Config -> IO ()
main' cfg@(Config {..}) = do
  let emptyObject = Aeson.object []

  renderTemplates templateDir emptyObject
    >>= guard . all id

  renderPdf cfg

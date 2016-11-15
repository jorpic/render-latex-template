

module Main where

import           Control.Monad (guard)
import           Control.Concurrent (forkIO)

import qualified Data.Aeson as Aeson
import qualified Data.Configurator as Config
import           Data.GUID as GUID

import           System.Directory
  ( copyFile
  , removeDirectoryRecursive
  , createDirectoryIfMissing
  )
import qualified System.Environment as Env
import           System.FilePath ((</>))
import           System.Path (copyDir)

import Config
import Render
import Utils


type DocumentId = String
type TemplateId = String


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

      initWorkspace config
      main' config
    _ -> report $ "Usage: " ++ prog ++ " <config.conf>"


main' :: Config -> IO ()
main' cfg@(Config {..}) = do
  let emptyObject = Aeson.object []
  let templateId = "1"
  docId <- job cfg templateId emptyObject
  putStrLn docId
  return ()



-- | Create directories and such
initWorkspace :: Config -> IO ()
initWorkspace (Config {..}) = do
  report "initWorkspace: create directories"
  createDirectoryIfMissing True templateDir
  createDirectoryIfMissing True workDir
  createDirectoryIfMissing True docDir
  report "initWorkspace: done"



-- FIXME: forkIO? explain
job :: Config -> TemplateId -> Aeson.Value -> IO DocumentId
job cfg@(Config {..}) tplId obj = do
  let tplPath = templateDir </> tplId
  -- FIXME: check if template exists

  docId <- GUID.genString
  -- copy template to workdir
  let workPath = workDir </> docId
  createDirectoryIfMissing True workPath
  copyDir tplPath workPath
  do -- forkIO
    renderTemplates workPath obj
      >>= guard . all id

    renderPdf cfg workPath

    -- TODO: hash as filename
    copyFile
      (workPath </> "template.pdf")
      (docDir </> docId ++ ".pdf")

    -- drop work dir
    removeDirectoryRecursive workPath

  return docId


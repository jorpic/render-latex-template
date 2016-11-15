
module Job
  ( initWorkspace
  , startJob
  , TemplateId
  , DocumentId
  ) where


import           Control.Monad (guard)
import           Control.Concurrent (forkIO)
import qualified Data.Aeson as Aeson
import           Data.GUID as GUID

import System.FilePath ((</>))
import System.Path (copyDir)
import System.Directory
  ( copyFile
  , removeDirectoryRecursive
  , createDirectoryIfMissing
  )

import Config
import Render
import Utils (report)


type DocumentId = String
type TemplateId = String


-- | Create directories and such
initWorkspace :: Config -> IO ()
initWorkspace (Config {..}) = do
  report "initWorkspace: create directories"
  createDirectoryIfMissing True templateDir
  createDirectoryIfMissing True workDir
  createDirectoryIfMissing True docDir
  report "initWorkspace: done"



-- FIXME: forkIO? explain
startJob :: Config -> TemplateId -> Aeson.Value -> IO DocumentId
startJob cfg@(Config {..}) tplId obj = do
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

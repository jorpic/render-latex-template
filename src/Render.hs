
module Render
  ( renderTemplates
  , renderPdf
  ) where


import           Control.Monad (filterM)
import           Control.Lens ((.~))

import           Data.List (isSuffixOf)
import qualified Data.Text.Lazy.IO as TL
import           Data.Aeson (Value(..))
import qualified Text.EDE as EDE

import           System.Directory (listDirectory, doesFileExist)
import           System.FilePath ((</>))
import qualified System.Process as P
import           System.Exit

import Config
import Utils


-- FIXME: move ".tex" to config?
-- | Render *.tex templates in `dir`.
renderTemplates :: FilePath -> Value -> IO [Bool]
renderTemplates dir (Object obj)
  = forEachFile ".tex" dir (\filePath -> do
    putStrLn filePath
    EDE.parseFileWith latexSyntax filePath >>= \case
      EDE.Failure err -> report (show err) >> return False
      EDE.Success tpl -> case EDE.eitherRender tpl obj of
        Left err  -> report err >> return False
        Right txt -> TL.writeFile filePath txt >> return True
    )
renderTemplates _ _val = report "Invalid json data" >> return []


renderPdf :: Config -> IO ()
renderPdf (Config {..}) = do
  (code, _sOut, _sErr) <- P.readCreateProcessWithExitCode
    (P.shell renderCmd) {P.cwd = Just templateDir}
    ""
  case code of
    ExitFailure _err -> return ()
    ExitSuccess -> return ()


-- | Shallow scan `dir` for files with `ext` and perform `op` on each of them.
forEachFile :: String -> FilePath -> (FilePath -> IO a) -> IO [a]
forEachFile ext dir op
  =   listDirectory dir
  >>= return . map (dir </>) . filter (isSuffixOf ext)
  >>= filterM doesFileExist
  >>= mapM op


latexSyntax :: EDE.Syntax
latexSyntax
  = EDE.delimInline  .~ ("\\param{", "}")
  $ EDE.delimPragma  .~ ("%% !EDE:", "\n")
  $ EDE.delimBlock   .~ ("%% %EDE:", "\n")
  $ EDE.delimComment .~ ("%% #EDE:", "\n")
  $ EDE.defaultSyntax


{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad (guard, filterM)
import           Control.Lens ((.~))

import           Data.List (isSuffixOf)
import qualified Data.Aeson as Aeson
import qualified Data.Configurator as Config


import           System.IO (hPutStrLn, stderr)
import           System.Directory (listDirectory, doesFileExist)
import           System.FilePath ((</>))
import qualified System.Environment as Env
import qualified System.Process as P
import           System.Exit

import qualified Data.Text.Lazy.IO as TL
import qualified Text.EDE as EDE

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
    _ -> report $ "Usage: " ++ prog ++ " <config.conf>"


main' :: Config -> IO ()
main' (Config {..}) = do
  let emptyObject = EDE.fromPairs []

  renderTemplates templateDir emptyObject
    >>= guard . all id

  (code, _sOut, _sErr) <- P.readCreateProcessWithExitCode
    (P.shell renderCmd) {P.cwd = Just templateDir}
    ""
  case code of
    ExitFailure _err -> return ()
    ExitSuccess -> return ()


-- FIXME: move ".tex" to config?
-- | Render *.tex templates in `dir`.
renderTemplates :: FilePath -> Aeson.Object -> IO [Bool]
renderTemplates dir obj
  = forEachFile ".tex" dir (\filePath -> do
    putStrLn filePath
    EDE.parseFileWith latexSyntax filePath >>= \case
      EDE.Failure err -> report (show err) >> return False
      EDE.Success tpl -> case EDE.eitherRender tpl obj of
        Left err  -> report err >> return False
        Right txt -> TL.writeFile filePath txt >> return True
    )


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


is :: Bool -> Bool
is = id


report :: String -> IO ()
report = hPutStrLn stderr

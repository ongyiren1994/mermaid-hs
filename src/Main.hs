{-# LANGUAGE TypeApplications #-}

module Main where

import Diagram
import Main.Utf8 (withUtf8)
import Options.Applicative as O
import qualified Shower
import Text.Megaparsec as M

opt :: O.Parser FilePath
opt =
  strOption
    ( long "file with mermaid syntax"
        <> metavar "FILE"
        <> short 'f'
    )

main :: IO ()
main = do
  withUtf8 $ do
    let opts = info (opt <**> helper) (fullDesc <> progDesc "Parse mermaid syntax inside a file")
    file <- execParser opts
    content <- readFileText file
    let res = parse (pDiagram <* eof) file content
    Shower.printer res

{-# LANGUAGE TypeApplications #-}

module Main where

import FlowChart
import Main.Utf8 (withUtf8)
import Options.Applicative as O
import Text.Megaparsec

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
    content <- readFile file
    parseTest (pDiagram <* eof) (fromString content)

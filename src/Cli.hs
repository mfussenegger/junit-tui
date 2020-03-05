
module Cli
  ( parseArgs,
    Source (..),
  )
where

import Options.Applicative


data Source
  = Folder String
  | File String

folderSource :: Parser Source
folderSource = Folder <$> option str (long "directory")

fileSource :: Parser Source
fileSource = File <$> option str (long "file")

sourceParser :: Parser Source
sourceParser = folderSource <|> fileSource

parseArgs :: IO Source
parseArgs = execParser opts
  where
    opts = info (sourceParser <**> helper)
      ( fullDesc <> progDesc "Junit Result Terminal viewer")

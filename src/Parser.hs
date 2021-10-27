module Parser where

import Text.Megaparsec hiding (State)

type Parser = Parsec Void Text
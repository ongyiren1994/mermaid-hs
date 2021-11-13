module Parser where

import Text.Megaparsec hiding (State)
import Text.Megaparsec as M
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    M.empty

sc' :: Parser ()
sc' =
  L.space
    (void $ M.some (char ' ' <|> char '\t'))
    (L.skipLineComment "//")
    M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme sc'

symbol :: Text -> Parser Text
symbol = L.symbol sc

pCheckIndent :: Pos -> Parser Pos
pCheckIndent = L.indentGuard sc EQ

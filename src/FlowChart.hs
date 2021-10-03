{-# LANGUAGE RecordWildCards #-}

import Algebra.Graph.Labelled as LG
import Text.Megaparsec hiding (State)
import Text.Megaparsec as M
import Text.Megaparsec.Char (alphaNumChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Orientation = TB | TD | BT | RL | LR deriving (Eq, Show)

data Diagram
  = FlowChart
      { orientation :: Orientation,
        graph :: [Graph (Style, Maybe String) [(String, Maybe (Bracket, String))]]
      }
  | Others
  deriving (Eq, Show)

type Style = Text

data DiagramType = FlowChart' | Others' deriving (Eq, Show)

data Bracket = Square | Arrow | Paren deriving (Eq, Show, Ord)

sc :: Parser ()
sc =
  L.space
    space1
    M.empty
    M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

someBracket :: Text -> Text -> Parser a -> Parser a
someBracket open close = between (symbol open) (symbol close)

pOrientation :: Parser Orientation
pOrientation =
  choice
    [ TB <$ string "TB",
      TD <$ string "TD",
      BT <$ string "BT",
      RL <$ string "RL",
      LR <$ string "LR"
    ]

pLink :: Parser (Style, Maybe String)
pLink =
  choice
    [ do
        void $ string "-->"
        pure ("-->", Nothing),
      do
        void $ string "---"
        pure ("---", Nothing),
      do
        void $ lexeme "--"
        content <- lexeme (M.some alphaNumChar)
        void $ lexeme "---"
        pure ("-- ---", Just content),
      do
        void $ lexeme "=="
        content <- lexeme (M.some alphaNumChar)
        void $ lexeme "==>"
        pure ("== ==>", Just content)
    ]

pMultiNode :: Parser Text
pMultiNode = string "&"

pBracket :: Parser (Bracket, String)
pBracket =
  choice
    [ do
        content <- someBracket "{" "}" (M.some alphaNumChar)
        pure (Arrow, content),
      do
        content <- someBracket "(" ")" (M.some alphaNumChar)
        pure (Paren, content),
      do
        content <- someBracket "[" "]" (M.some alphaNumChar)
        pure (Square, content)
    ]

pDiagramType :: Parser DiagramType
pDiagramType =
  choice
    [ FlowChart' <$ string "flowchart",
      Others' <$ string "PLACEHOLDER"
    ]

pDiagram :: Parser Diagram
pDiagram = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      chart <- pDiagramType
      case chart of
        FlowChart' -> do
          void $ lexeme " "
          orientation <- pOrientation
          return (L.IndentSome Nothing (return . (\a -> FlowChart {orientation = orientation, graph = concat a})) pGraph)
        Others' -> return (L.IndentNone Others)

pVertex :: Parser String
pVertex = lexeme (M.some alphaNumChar) <?> "vertex"

pGraph :: Parser [Graph (Style, Maybe String) [(String, Maybe (Bracket, String))]]
pGraph = do
  vertexL <- pVertex
  maybeBracketContentL <- optional pBracket
  link <- lexeme pLink
  vertexR <- pVertex
  maybeBracketContentR <- optional pBracket
  pGraphRecursive [edge link [(vertexL, maybeBracketContentL)] [(vertexR, maybeBracketContentR)]]

pGraphRecursive :: [Graph (Style, Maybe String) [(String, Maybe (Bracket, String))]] -> Parser [Graph (Style, Maybe String) [(String, Maybe (Bracket, String))]]
pGraphRecursive graph = do
  link <- optional $ lexeme pLink
  case link of
    Nothing -> do
      branch <- optional $ lexeme pMultiNode
      case branch of
        Just _ -> do
          vertexR <- pVertex
          maybeBracketContentR <- optional pBracket
          case graph of
            (Connect x y (Vertex b)) : xs -> do
              pGraphRecursive $ Connect x y (Vertex ((++) b [(vertexR, maybeBracketContentR)])) : xs
            _ -> return []
        Nothing -> return graph
    Just link' -> do
      vertexR <- pVertex
      maybeBracketContentR <- optional pBracket
      case graph of
        (Connect _ _ (Vertex a)) : _ -> pGraphRecursive $ edge link' a [(vertexR, maybeBracketContentR)] : graph
        _ -> return []
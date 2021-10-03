{-# LANGUAGE RecordWildCards #-}

module FlowChart where
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
        graph :: [Graph (LinkStyle, Maybe LinkText) [(NodeId, Maybe (Bracket, NodeName))]]
      }
  | Others
  deriving (Eq, Show)

type LinkStyle = Text

type LinkText = Text

type NodeId = Text

type NodeName = Text

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

pLink :: Parser (LinkStyle, Maybe LinkText)
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
        content <- lexeme (fromString <$> M.some alphaNumChar)
        void $ lexeme "---"
        pure ("-- ---", Just content),
      do
        void $ lexeme "=="
        content <- lexeme (fromString <$> M.some alphaNumChar)
        void $ lexeme "==>"
        pure ("== ==>", Just content)
    ]

pMultiNode :: Parser Text
pMultiNode = string "&"

pBracket :: Parser (Bracket, Text)
pBracket =
  choice
    [ do
        content <- someBracket "{" "}" (fromString <$> M.some alphaNumChar)
        pure (Arrow, content),
      do
        content <- someBracket "(" ")" (fromString <$> M.some alphaNumChar)
        pure (Paren, content),
      do
        content <- someBracket "[" "]" (fromString <$> M.some alphaNumChar)
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

pVertex :: Parser Text
pVertex = lexeme (fromString <$> M.some alphaNumChar) <?> "vertex"

pGraph :: Parser [Graph (LinkStyle, Maybe LinkText) [(NodeId, Maybe (Bracket, NodeName))]]
pGraph = do
  vertexL <- pVertex
  maybeBracketContentL <- optional pBracket
  link <- lexeme pLink
  vertexR <- pVertex
  maybeBracketContentR <- optional pBracket
  pGraphRecursive [edge link [(vertexL, maybeBracketContentL)] [(vertexR, maybeBracketContentR)]]

pGraphRecursive :: [Graph (LinkStyle, Maybe LinkText) [(NodeId, Maybe (Bracket, NodeName))]] -> Parser [Graph (LinkStyle, Maybe LinkText) [(NodeId, Maybe (Bracket, NodeName))]]
pGraphRecursive z@((Connect x y (Vertex a)) : xs) = do
  link <- optional $ lexeme pLink
  case link of
    Nothing -> do
      branch <- optional $ lexeme pMultiNode
      case branch of
        Just _ -> do
          vertexR <- pVertex
          maybeBracketContentR <- optional pBracket
          pGraphRecursive $ Connect x y (Vertex ((++) a [(vertexR, maybeBracketContentR)])) : xs
        Nothing -> return z
    Just link' -> do
      vertexR <- pVertex
      maybeBracketContentR <- optional pBracket
      pGraphRecursive $ edge link' a [(vertexR, maybeBracketContentR)] : z
pGraphRecursive _ = return []
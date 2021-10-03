{-# LANGUAGE RecordWildCards #-}

module FlowChart where

import Algebra.Graph.Labelled as LG
import Relude.Extra.Foldable1
import System.Directory.Internal.Prelude ()
import Text.Megaparsec hiding (State)
import Text.Megaparsec as M
import Text.Megaparsec.Char (alphaNumChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Orientation = TB | TD | BT | RL | LR deriving (Eq, Show)

data Diagram
  = FlowChart
      { orientation :: Orientation,
        graph :: FlowChartGraph
      }
  | Others
  deriving (Eq, Show)

type FlowChartGraph = Graph Edge [Node]

data Edge = Edge
  { edgeStyle :: Text,
    edgeLabel :: Maybe Text
  }
  deriving (Eq, Show)

instance Semigroup Edge where
  (<>) (Edge a b) (Edge c d) = (Edge (a <> c) (b <> d))

instance Monoid Edge where
  mempty = Edge "" Nothing

data Node = Node
  { nodeId :: Text,
    nodeLabel :: Maybe (Bracket, Text)
  }
  deriving (Eq, Show, Ord)

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

pLink :: Parser Edge
pLink =
  choice
    [ do
        void $ string "-->"
        pure $ Edge "-->" Nothing,
      do
        void $ string "---"
        pure $ Edge "---" Nothing,
      do
        void $ lexeme "--"
        content <- lexeme (fromString <$> M.some alphaNumChar)
        void $ lexeme "---"
        pure $ Edge "-- ---" $ Just content,
      do
        void $ lexeme "=="
        content <- lexeme (fromString <$> M.some alphaNumChar)
        void $ lexeme "==>"
        pure $ Edge "== ==>" $ Just content
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
          return (L.IndentSome Nothing (return . (\a -> FlowChart {orientation = orientation, graph = foldl1' overlay (fromList (concat a))})) pGraph)
        Others' -> return (L.IndentNone Others)

pVertex :: Parser Text
pVertex = lexeme (fromString <$> M.some alphaNumChar) <?> "vertex"

pGraph :: Parser [FlowChartGraph]
pGraph = do
  vertexL <- pVertex
  maybeBracketContentL <- optional pBracket
  pGraphRecursive [vertex [Node vertexL maybeBracketContentL]]

pGraphRecursive :: [FlowChartGraph] -> Parser [FlowChartGraph]
pGraphRecursive graphs = do
  link <- optional $ lexeme pLink
  case link of
    Nothing -> do
      branch <- optional $ lexeme pMultiNode
      case branch of
        Just _ -> do
          vertexR <- pVertex
          maybeBracketContentR <- optional pBracket
          case graphs of
            [Vertex a] -> pGraphRecursive [Vertex ((++) a [Node vertexR maybeBracketContentR])]
            (Connect x y (Vertex a)) : xs -> pGraphRecursive $ Connect x y (Vertex ((++) a [Node vertexR maybeBracketContentR])) : xs
            _ -> return graphs
        Nothing -> return graphs
    Just link' -> do
      vertexR <- pVertex
      maybeBracketContentR <- optional pBracket
      case graphs of
        [Vertex a] -> pGraphRecursive [edge link' a [Node vertexR maybeBracketContentR]]
        z@((Connect _ _ (Vertex a)) : _) -> pGraphRecursive $ edge link' a [Node vertexR maybeBracketContentR] : z
        _ -> return graphs

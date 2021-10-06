{-# LANGUAGE DeriveAnyClass #-}

module FlowChart where

import Algebra.Graph.Labelled as LG
import Relude.Extra.Foldable1
import System.Directory.Internal.Prelude ()
import Text.Megaparsec hiding (State)
import Text.Megaparsec as M
import Text.Megaparsec.Char (alphaNumChar, char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Diagram
  = FlowChart
      { orientation :: Orientation,
        graph :: FlowChartGraph
      }
  | Others
  deriving (Eq, Show)

data Orientation = TB | TD | BT | RL | LR deriving (Eq, Show)

type FlowChartGraph = Graph (Maybe Edge) [Node]

data Edge = Edge
  { edgeStyle :: Maybe Text,
    edgeLabel :: Maybe EdgeLabel
  }
  deriving (Eq, Show)

instance Semigroup Edge where
  (<>) (Edge a b) (Edge c d) = Edge (a <> c) (b <> d)

instance Monoid Edge where
  mempty = Edge Nothing Nothing

instance Semigroup EdgeLabel where
  (<>) (EdgeLabel a) (EdgeLabel b) = EdgeLabel (a <> b)

data Node = Node
  { nodeId :: NodeId,
    nodeShape :: Maybe Shape,
    nodeLabel :: Maybe NodeLabel
  }
  deriving (Eq, Show, Ord)

data Shape = Default | Rhombus | Round | Stadium | Subroutine | Cylindrical | Circle | Asymmetric | Hexagon | Parallelogram | ParallelogramAlt | Trapezoid | TrapezoidAlt deriving (Eq, Show, Ord)

newtype NodeLabel = NodeLabel {unNodeLabel :: Text} deriving (Eq, Show, Ord)

newtype NodeId = NodeId {unNodeId :: Text} deriving (Eq, Show, Ord)

newtype EdgeLabel = EdgeLabel {unEdgeLabel :: Text} deriving (Eq, Show)

newtype EdgeStyle = EdgeStyle {unEdgeStyle :: Text} deriving (Eq, Show)

data DiagramType = FlowChartType | OtherType deriving (Eq, Show)

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

someShape :: Text -> Text -> Parser a -> Parser a
someShape open close = between (symbol open) (symbol close)

pDiagramType :: Parser DiagramType
pDiagramType =
  choice
    [ FlowChartType <$ string "flowchart",
      OtherType <$ string "PLACEHOLDER"
    ]

pOrientation :: Parser Orientation
pOrientation =
  choice
    [ TB <$ string "TB",
      TD <$ string "TD",
      BT <$ string "BT",
      RL <$ string "RL",
      LR <$ string "LR"
    ]

pVertex :: Parser Text
pVertex = lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!')) <?> "vertex"

pShape :: Parser (Shape, NodeLabel)
pShape =
  choice
    [ do
        content <- someShape "([" "])" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Stadium, NodeLabel content),
      do
        content <- someShape "[[" "]]" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Subroutine, NodeLabel content),
      do
        content <- someShape "((" "))" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Cylindrical, NodeLabel content),
      do
        content <- someShape "[(" ")]" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Circle, NodeLabel content),
      do
        content <- someShape ">" "]" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Asymmetric, NodeLabel content),
      do
        content <- someShape "{{" "}}" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Hexagon, NodeLabel content),
      do
        void $ lexeme $ "[/"
        content <- fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!')
        closeShapeA <- optional $ lexeme $ "/]"
        case closeShapeA of
          Just _ -> pure (Parallelogram, NodeLabel content)
          Nothing -> do
            void $ lexeme "\\]"
            pure (Trapezoid, NodeLabel content),
      do
        void $ lexeme $ "[\\"
        content <- fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!')
        closeShapeA <- optional $ lexeme $ "\\]"
        case closeShapeA of
          Just _ -> pure (ParallelogramAlt, NodeLabel content)
          Nothing -> do
            void $ lexeme "/]"
            pure (TrapezoidAlt, NodeLabel content),
      do
        content <- someShape "{" "}" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Rhombus, NodeLabel content),
      do
        content <- someShape "(" ")" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Round, NodeLabel content),
      do
        content <- someShape "[" "]" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Default, NodeLabel content)
    ]

pLink :: Parser Edge
pLink =
  choice
    [ do
        void $ lexeme $ string "-->"
        content <- optional $ do
          void $ lexeme "|"
          lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        case content of
          Just x -> do
            void $ choice [string "| ", string "|"]
            pure $ Edge (Just "--> ||") (Just (EdgeLabel x))
          Nothing -> pure $ Edge (Just "-->") Nothing,
      do
        void $ lexeme $ string "---"
        content <- optional $ do
          void $ lexeme "|"
          lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        case content of
          Just x -> do
            void $ choice [string "| ", string "|"]
            pure $ Edge (Just "--- ||") (Just (EdgeLabel x))
          Nothing -> pure $ Edge (Just "---") Nothing,
      do
        void $ lexeme "--o"
        pure $ Edge (Just "--o") Nothing,
      do
        void $ lexeme "--x"
        pure $ Edge (Just "--x") Nothing,
      do
        void $ lexeme "--"
        content <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        void $ string "--"
        extraDash <- optional $ lexeme $ string "-"
        case extraDash of
          Just _ -> do
            pure $ Edge (Just "-- ---") $ Just (EdgeLabel content)
          Nothing -> do
            void $ lexeme $ string ">"
            pure $ Edge (Just "-- --") $ Just (EdgeLabel content),
      do
        void $ lexeme "==>"
        pure $ Edge (Just "==>") Nothing,
      do
        void $ lexeme "=="
        content <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        void $ lexeme "==>"
        pure $ Edge (Just "== ==>") $ Just (EdgeLabel content),
      do
        void $ lexeme "o--o"
        pure $ Edge (Just "o--o") Nothing,
      do
        void $ lexeme "x--x"
        pure $ Edge (Just "x--x") Nothing,
      do
        void $ lexeme "<-->"
        pure $ Edge (Just "<-->") Nothing,
      do
        void $ lexeme "-."
        content <- optional $ lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        case content of
          Just _ -> do
            void $ lexeme ".->"
            pure $ Edge (Just "-.->") (EdgeLabel <$> content)
          Nothing -> do
            void $ lexeme "->"
            pure $ Edge (Just "-.->") (EdgeLabel <$> content)
    ]

pMultiNode :: Parser Text
pMultiNode = string "&"

pDiagram :: Parser Diagram
pDiagram = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      chart <- pDiagramType
      case chart of
        FlowChartType -> do
          void $ lexeme " "
          orientation <- pOrientation
          return (L.IndentSome Nothing (return . (FlowChart orientation . foldl1' overlay . fromList . concat)) pFlowChartGraph)
        OtherType -> return (L.IndentNone Others)

pFlowChartGraph :: Parser [FlowChartGraph]
pFlowChartGraph = do
  vertexL <- pVertex
  maybeShapeLabel <- optional pShape
  let (shape, label) = spiltJust maybeShapeLabel
  pFlowChartGraphRecursive [vertex [Node (NodeId vertexL) shape label]]

pFlowChartGraphRecursive :: [FlowChartGraph] -> Parser [FlowChartGraph]
pFlowChartGraphRecursive graphs = do
  link <- optional $ lexeme pLink
  case link of
    Nothing -> do
      branch <- optional $ lexeme pMultiNode
      case branch of
        Just _ -> do
          vertexR <- pVertex
          maybeShapeLabel <- optional pShape
          let (shape, label) = spiltJust maybeShapeLabel
          case graphs of
            [Vertex a] -> pFlowChartGraphRecursive [Vertex ((++) a [Node (NodeId vertexR) shape label])]
            (Connect x y (Vertex a)) : xs -> pFlowChartGraphRecursive $ Connect x y (Vertex ((++) a [Node (NodeId vertexR) shape label])) : xs
            _ -> return graphs
        Nothing -> return graphs
    Just link' -> do
      vertexR <- pVertex
      maybeShapeLabel <- optional pShape
      let (shape, label) = spiltJust maybeShapeLabel
      case graphs of
        [Vertex a] -> pFlowChartGraphRecursive [edge (Just link') a [Node (NodeId vertexR) shape label]]
        z@((Connect _ _ (Vertex a)) : _) -> pFlowChartGraphRecursive $ edge (Just link') a [Node (NodeId vertexR) shape label] : z
        _ -> return graphs

spiltJust :: Maybe (a, b) -> (Maybe a, Maybe b)
spiltJust (Just (a, b)) = (Just a, Just b)
spiltJust Nothing = (Nothing, Nothing)
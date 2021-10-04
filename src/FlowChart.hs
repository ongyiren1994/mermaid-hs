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

type FlowChartGraph = Graph Edge [Node]

data Edge = Edge
  { edgeStyle :: Text,
    edgeLabel :: Maybe Text
  }
  deriving (Eq, Show)

instance Semigroup Edge where
  (<>) (Edge a b) (Edge c d) = Edge (a <> c) (b <> d)

instance Monoid Edge where
  mempty = Edge "" Nothing

data Node = Node
  { nodeId :: Text,
    nodeLabel :: Maybe (Bracket, Text)
  }
  deriving (Eq, Show, Ord)

data Bracket = Default | Rhombus | Round | Stadium | Subroutine | Cylindrical | Circle | Asymmetric | Hexagon | Parallelogram | ParallelogramAlt | Trapezoid | TrapezoidAlt deriving (Eq, Show, Ord)

data DiagramType = FlowChart' | Others' deriving (Eq, Show)

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

someBracket :: Text -> Text -> Parser a -> Parser a
someBracket open close = between (symbol open) (symbol close)

pDiagramType :: Parser DiagramType
pDiagramType =
  choice
    [ FlowChart' <$ string "flowchart",
      Others' <$ string "PLACEHOLDER"
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

pBracket :: Parser (Bracket, Text)
pBracket =
  choice
    [ do
        content <- someBracket "([" "])" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Stadium, content),
      do
        content <- someBracket "[[" "]]" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Subroutine, content),
      do
        content <- someBracket "((" "))" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Cylindrical, content),
      do
        content <- someBracket "[(" ")]" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Circle, content),
      do
        content <- someBracket ">" "]" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Asymmetric, content),
      do
        content <- someBracket "{{" "}}" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Hexagon, content),
      do
        void $ lexeme $ "[/"
        content <- fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!')
        closeBracketA <- optional $ lexeme $ "/]"
        case closeBracketA of
          Just _ -> pure (Parallelogram, content)
          Nothing -> do
            void $ lexeme "\\]"
            pure (Trapezoid, content),
      do
        void $ lexeme $ "[\\"
        content <- fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!')
        closeBracketA <- optional $ lexeme $ "\\]"
        case closeBracketA of
          Just _ -> pure (ParallelogramAlt, content)
          Nothing -> do
            void $ lexeme "/]"
            pure (TrapezoidAlt, content),
      do
        content <- someBracket "{" "}" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Rhombus, content),
      do
        content <- someBracket "(" ")" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Round, content),
      do
        content <- someBracket "[" "]" (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        pure (Default, content)
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
            pure $ Edge "--> ||" (Just x)
          Nothing -> pure $ Edge "-->" Nothing,
      do
        void $ lexeme $ string "---"
        content <- optional $ do
          void $ lexeme "|"
          lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        case content of
          Just x -> do
            void $ choice [string "| ", string "|"]
            pure $ Edge "--- ||" (Just x)
          Nothing -> pure $ Edge "---" Nothing,
      do
        void $ lexeme "--o"
        pure $ Edge "--o" Nothing,
      do
        void $ lexeme "--x"
        pure $ Edge "--x" Nothing,
      do
        void $ lexeme "--"
        content <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        void $ string "--"
        extraDash <- optional $ lexeme $ string "-"
        case extraDash of
          Just _ -> do
            pure $ Edge "-- ---" $ Just content
          Nothing -> do
            void $ lexeme $ string ">"
            pure $ Edge "-- --" $ Just content,
      do
        void $ lexeme "==>"
        pure $ Edge "==>" Nothing,
      do
        void $ lexeme "=="
        content <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        void $ lexeme "==>"
        pure $ Edge "== ==>" $ Just content,
      do
        void $ lexeme "o--o"
        pure $ Edge "o--o" Nothing,
      do
        void $ lexeme "x--x"
        pure $ Edge "x--x" Nothing,
      do
        void $ lexeme "<-->"
        pure $ Edge "<-->" Nothing,
      do
        void $ lexeme "-."
        content <- optional $ lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!'))
        case content of
          Just _ -> do
            void $ lexeme ".->"
            pure $ Edge "-.->" content
          Nothing -> do
            void $ lexeme "->"
            pure $ Edge "-.->" content
    ]

pMultiNode :: Parser Text
pMultiNode = string "&"

pDiagram :: Parser Diagram
pDiagram = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      chart <- pDiagramType
      case chart of
        FlowChart' -> do
          void $ lexeme " "
          orientation <- pOrientation
          return (L.IndentSome Nothing (return . (FlowChart orientation . foldl1' overlay . fromList . concat)) pFlowChartGraph)
        Others' -> return (L.IndentNone Others)

pFlowChartGraph :: Parser [FlowChartGraph]
pFlowChartGraph = do
  vertexL <- pVertex
  maybeBracketContentL <- optional pBracket
  pFlowChartGraphRecursive [vertex [Node vertexL maybeBracketContentL]]

pFlowChartGraphRecursive :: [FlowChartGraph] -> Parser [FlowChartGraph]
pFlowChartGraphRecursive graphs = do
  link <- optional $ lexeme pLink
  case link of
    Nothing -> do
      branch <- optional $ lexeme pMultiNode
      case branch of
        Just _ -> do
          vertexR <- pVertex
          maybeBracketContentR <- optional pBracket
          case graphs of
            [Vertex a] -> pFlowChartGraphRecursive [Vertex ((++) a [Node vertexR maybeBracketContentR])]
            (Connect x y (Vertex a)) : xs -> pFlowChartGraphRecursive $ Connect x y (Vertex ((++) a [Node vertexR maybeBracketContentR])) : xs
            _ -> return graphs
        Nothing -> return graphs
    Just link' -> do
      vertexR <- pVertex
      maybeBracketContentR <- optional pBracket
      case graphs of
        [Vertex a] -> pFlowChartGraphRecursive [edge link' a [Node vertexR maybeBracketContentR]]
        z@((Connect _ _ (Vertex a)) : _) -> pFlowChartGraphRecursive $ edge link' a [Node vertexR maybeBracketContentR] : z
        _ -> return graphs

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Diagram.FlowChart where

import Algebra.Graph.Labelled as LG
import Control.Lens.TH (makeLenses)
import Parser
import System.Directory.Internal.Prelude ()
import Text.Megaparsec as M
import Text.Megaparsec.Char (alphaNumChar, char, string)

data Orientation = TB | TD | BT | RL | LR deriving (Eq, Show, Generic)

data FlowChartGraph = FlowChartGraph
  { _flowChartOrientation :: Orientation,
    _flowChartGraph :: SubFlowChartGraph
  }
  deriving (Eq, Show, Generic)

type SubFlowChartGraph = Graph (Maybe Edge) Node

data Edge = Edge
  { _edgeStyle :: Maybe Text,
    _edgeLabel :: Maybe EdgeLabel
  }
  deriving (Eq, Show, Generic)

instance Semigroup Edge where
  (<>) (Edge a b) (Edge c d) = Edge (a <> c) (b <> d)

instance Semigroup EdgeLabel where
  (<>) (EdgeLabel a) (EdgeLabel b) = EdgeLabel (a <> b)

data Node = Node
  { _nodeId :: NodeId,
    _nodeShape :: Maybe Shape,
    _nodeLabel :: Maybe NodeLabel
  }
  deriving (Eq, Show, Ord, Generic)

data Shape = Default | Rhombus | Round | Stadium | Subroutine | Cylindrical | Circle | Asymmetric | Hexagon | Parallelogram | ParallelogramAlt | Trapezoid | TrapezoidAlt deriving (Eq, Show, Ord, Generic)

newtype NodeLabel = NodeLabel {_unNodeLabel :: Text} deriving (Eq, Show, Ord, Generic, IsString)

newtype NodeId = NodeId {_unNodeId :: Text} deriving (Eq, Show, Ord, Generic, IsString)

newtype EdgeLabel = EdgeLabel {_unEdgeLabel :: Text} deriving (Eq, Show, Generic, IsString)

newtype EdgeStyle = EdgeStyle {_unEdgeStyle :: Text} deriving (Eq, Show, Generic, IsString)

makeLenses ''Orientation

makeLenses ''Edge

makeLenses ''Node

makeLenses ''Shape

makeLenses ''NodeLabel

makeLenses ''NodeId

makeLenses ''EdgeLabel

makeLenses ''EdgeStyle

instance IsString Node where
  fromString s = Node (fromString s) Nothing Nothing

instance IsString Edge where
  fromString s = Edge (Just (fromString s)) Nothing

someShape :: Text -> Text -> Parser a -> Parser a
someShape open close = between (symbol open) (symbol close)

pOrientation :: Parser Orientation
pOrientation =
  choice
    [ TB <$ string "TB",
      TD <$ string "TD",
      BT <$ string "BT",
      RL <$ string "RL",
      LR <$ string "LR"
    ]

pVertex :: Parser NodeId
pVertex = NodeId <$> lexeme (fromString <$> M.some (alphaNumChar <|> char '?' <|> char '!')) <?> "vertex"

pShape :: Parser (Shape, NodeLabel)
pShape =
  choice
    [ do
        content <- someShape "([" "])" (fromString <$> M.some pText)
        pure (Stadium, NodeLabel content),
      do
        content <- someShape "[[" "]]" (fromString <$> M.some pText)
        pure (Subroutine, NodeLabel content),
      do
        content <- someShape "((" "))" (fromString <$> M.some pText)
        pure (Cylindrical, NodeLabel content),
      do
        content <- someShape "[(" ")]" (fromString <$> M.some pText)
        pure (Circle, NodeLabel content),
      do
        content <- someShape ">" "]" (fromString <$> M.some pText)
        pure (Asymmetric, NodeLabel content),
      do
        content <- someShape "{{" "}}" (fromString <$> M.some pText)
        pure (Hexagon, NodeLabel content),
      do
        void $ lexeme "[/"
        content <- fromString <$> M.some pText
        closeShapeA <- optional $ lexeme "/]"
        case closeShapeA of
          Just _ -> pure (Parallelogram, NodeLabel content)
          Nothing -> do
            void $ lexeme "\\]"
            pure (Trapezoid, NodeLabel content),
      do
        void $ lexeme "[\\"
        content <- fromString <$> M.some pText
        closeShapeA <- optional $ lexeme "\\]"
        case closeShapeA of
          Just _ -> pure (ParallelogramAlt, NodeLabel content)
          Nothing -> do
            void $ lexeme "/]"
            pure (TrapezoidAlt, NodeLabel content),
      do
        content <- someShape "{" "}" (fromString <$> M.some pText)
        pure (Rhombus, NodeLabel content),
      do
        content <- someShape "(" ")" (fromString <$> M.some pText)
        pure (Round, NodeLabel content),
      do
        content <- someShape "[" "]" (fromString <$> M.some pText)
        pure (Default, NodeLabel content)
    ]

pLink :: Parser Edge
pLink =
  choice
    [ do
        void $ lexeme $ string "-->"
        content <- optional $ do
          void $ lexeme "|"
          lexeme (fromString <$> M.some pText)
        case content of
          Just x -> do
            void $ choice [string "| ", string "|"]
            pure $ Edge (Just "--> ||") (Just (EdgeLabel x))
          Nothing -> pure $ Edge (Just "-->") Nothing,
      do
        void $ lexeme $ string "---"
        content <- optional $ do
          void $ lexeme "|"
          lexeme (fromString <$> M.some pText)
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
        content <- lexeme (fromString <$> M.some pText)
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
        content <- lexeme (fromString <$> M.some pText)
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
        content <- optional $ lexeme (fromString <$> M.some pText)
        case content of
          Just _ -> do
            void $ lexeme ".->"
            pure $ Edge (Just "-.->") (EdgeLabel <$> content)
          Nothing -> do
            void $ lexeme "->"
            pure $ Edge (Just "-.->") (EdgeLabel <$> content)
    ]

pNode :: Parser Node
pNode = do
  nodeId' <- pVertex
  maybeShapeLabel <- optional pShape
  let (shape, label') = spiltJust maybeShapeLabel
  return $ Node nodeId' shape label'

pExtraNode :: Parser Node
pExtraNode = lexeme "&" >> pNode

pNodes :: Parser [Node]
pNodes = do
  node <- pNode
  extraNodes <- M.many pExtraNode
  return $ node : extraNodes

pSubFlowChartGraphInit :: Parser [SubFlowChartGraph]
pSubFlowChartGraphInit = do
  nodes <- pNodes
  let isolatedNodes = vertices nodes
  pSubFlowChartGraph [isolatedNodes]

pSubFlowChartGraph :: [SubFlowChartGraph] -> Parser [SubFlowChartGraph]
pSubFlowChartGraph graphs = do
  link <- optional $ lexeme pLink
  case link of
    Nothing -> return graphs
    Just link' -> do
      nodes <- pNodes
      let isolatedNodes = vertices nodes
      case graphs of
        [] -> return graphs
        lastGraph : xs -> pSubFlowChartGraph $ isolatedNodes : connect (Just link') lastGraph isolatedNodes : xs

spiltJust :: Maybe (a, b) -> (Maybe a, Maybe b)
spiltJust (Just (a, b)) = (Just a, Just b)
spiltJust Nothing = (Nothing, Nothing)

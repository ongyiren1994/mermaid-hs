{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Diagram where

import Algebra.Graph.Labelled as LG
import Control.Lens.TH (makeLenses)
import FlowChart
import Parser
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L

data Diagram
  = FlowChart
      { _orientation :: Orientation,
        _graph :: FlowChartGraph
      }
  | Others
  deriving (Eq, Show, Generic)

makeLenses ''Diagram

data DiagramType = FlowChartType | OtherType deriving (Eq, Show, Generic)

makeLenses ''DiagramType

pDiagram :: Parser Diagram
pDiagram = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      chart <- pDiagramType
      case chart of
        FlowChartType -> do
          void $ lexeme " "
          orientation' <- pOrientation
          return (L.IndentSome Nothing (return . (FlowChart orientation' . overlays . fromList . concat)) pFlowChartGraphInit)
        OtherType -> return (L.IndentNone Others)

pDiagramType :: Parser DiagramType
pDiagramType =
  choice
    [ FlowChartType <$ string "flowchart",
      OtherType <$ string "PLACEHOLDER"
    ]

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Diagram where

import Algebra.Graph.Labelled as LG
import Control.Lens.TH (makeLenses)
import FlowChart
import GanttChart
import Parser
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L

data Diagram
  = FlowChart
      { _orientation :: Orientation,
        _flowGraph :: FlowChartGraph
      }
  | -- Replace [GExpr] with GanttChartGraph
    GanttChart {_ganttGraph :: [GExpr]}
  deriving (Eq, Show, Generic)

makeLenses ''Diagram

data DiagramType = FlowChartType | GanttChartType | OtherType deriving (Eq, Show, Generic)

makeLenses ''DiagramType

pDiagram :: Parser Diagram
pDiagram = pGanttChartDiagram <|> pFlowChartDiagram

pGanttChartDiagram :: Parser Diagram
pGanttChartDiagram = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      void $ string "gantt"
      return (L.IndentSome Nothing (return . GanttChart) pGanttChart)

pFlowChartDiagram :: Parser Diagram
pFlowChartDiagram = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      void $ string "flowchart"
      void $ lexeme " "
      orientation' <- pOrientation
      return (L.IndentSome Nothing (return . (FlowChart orientation' . overlays . fromList . concat)) pFlowChartGraphInit)

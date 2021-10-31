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
  | GanttChart {_ganttGraph :: GanttChartGraph}
  deriving (Eq, Show, Generic)

makeLenses ''Diagram

pDiagram :: Parser [Diagram]
pDiagram = many $ pGanttChartDiagram <|> pFlowChartDiagram

pGanttChartDiagram :: Parser Diagram
pGanttChartDiagram = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      void $ string "gantt"
      return (L.IndentSome Nothing (return . (GanttChart . gExprToGanttChartGraph)) pGanttChart)

pFlowChartDiagram :: Parser Diagram
pFlowChartDiagram = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      void $ string "flowchart"
      void $ lexeme " "
      orientation' <- pOrientation
      return (L.IndentSome Nothing (return . (FlowChart orientation' . overlays . fromList . concat)) pFlowChartGraphInit)

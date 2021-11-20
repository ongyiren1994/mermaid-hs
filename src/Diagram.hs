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

data Diagram = D_FlowChart FlowChartGraph | D_GanttChart GanttChartGraph
  deriving (Eq, Show, Generic)

makeLenses ''Diagram

pDiagram :: Parser [Diagram]
pDiagram = many $ fmap D_GanttChart pGanttChartGraph <|> fmap D_FlowChart pFlowChartGraph

pGanttChartGraph :: Parser GanttChartGraph
pGanttChartGraph = L.nonIndented sc p
  where
    p = do
      void $ lexeme "gantt"
      ref <- L.indentLevel
      ganttChartTitle <- pGanttChartTitle
      void $ pCheckIndent ref
      dateFormat <- pDateFormat
      void $ pCheckIndent ref
      axisFormat <- pAxisFormat
      sections <- pSections ref
      return $ GanttChartGraph ganttChartTitle dateFormat axisFormat sections

pFlowChartGraph :: Parser FlowChartGraph
pFlowChartGraph = L.nonIndented sc (L.indentBlock sc p)
  where
    p = do
      void $ string "flowchart"
      void $ lexeme " "
      orientation' <- pOrientation
      return (L.IndentSome Nothing (return . (FlowChartGraph orientation' . overlays . fromList . concat)) pSubFlowChartGraphInit)

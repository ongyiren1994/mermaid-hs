module PrettyPrinter.FlowChart where

import Algebra.Graph.Labelled as LG (edgeList)
import Diagram.FlowChart
import Prettyprinter

prNode :: Node -> Doc ann
prNode (Node nodeId nodeShape nodeLabel) = prNodeId nodeId <> prShape (nodeShape, nodeLabel)

prShape :: (Maybe Shape, Maybe NodeLabel) -> Doc ann
prShape (nodeShape, nodeLabel) = case (nodeShape, nodeLabel) of
  (Just Default, Just label) -> pretty ("[" :: Text) <> prNodeLabel label <> pretty ("]" :: Text)
  (Just Rhombus, Just label) -> pretty ("{" :: Text) <> prNodeLabel label <> pretty ("}" :: Text)
  (Just Round, Just label) -> pretty ("(" :: Text) <> prNodeLabel label <> pretty (")" :: Text)
  (Just Stadium, Just label) -> pretty ("([" :: Text) <> prNodeLabel label <> pretty ("])" :: Text)
  (Just Subroutine, Just label) -> pretty ("[[" :: Text) <> prNodeLabel label <> pretty ("]]" :: Text)
  (Just Cylindrical, Just label) -> pretty ("((" :: Text) <> prNodeLabel label <> pretty ("))" :: Text)
  (Just Circle, Just label) -> pretty ("[(" :: Text) <> prNodeLabel label <> pretty (")]" :: Text)
  (Just Asymmetric, Just label) -> pretty (">" :: Text) <> prNodeLabel label <> pretty ("]" :: Text)
  (Just Hexagon, Just label) -> pretty ("{{" :: Text) <> prNodeLabel label <> pretty ("}}" :: Text)
  (Just Parallelogram, Just label) -> pretty ("[/" :: Text) <> prNodeLabel label <> pretty ("/]" :: Text)
  (Just ParallelogramAlt, Just label) -> pretty ("[\\" :: Text) <> prNodeLabel label <> pretty ("\\]" :: Text)
  (Just Trapezoid, Just label) -> pretty ("[/" :: Text) <> prNodeLabel label <> pretty ("\\]" :: Text)
  (Just TrapezoidAlt, Just label) -> pretty ("[\\" :: Text) <> prNodeLabel label <> pretty ("/]" :: Text)
  _ -> pretty ("" :: Text)

prNodeLabel :: NodeLabel -> Doc ann
prNodeLabel (NodeLabel unNodeLabel) = pretty unNodeLabel

prNodeId :: NodeId -> Doc ann
prNodeId (NodeId unNodeId) = pretty unNodeId

prEdgeLabel :: EdgeLabel -> Doc ann
prEdgeLabel (EdgeLabel unEdgeLabel) = pretty unEdgeLabel

prEdge :: Edge -> Doc ann
prEdge (Edge edgeStyle edgeLabel) = case (edgeStyle, edgeLabel) of
  (EdgeA, Just label) -> pretty ("-->" :: Text) <> prEdgeLabel label <> pretty ("||" :: Text)
  (EdgeB, _) -> pretty ("-->" :: Text)
  (EdgeC, Just label) -> pretty ("---" :: Text) <> prEdgeLabel label <> pretty ("||" :: Text)
  (EdgeD, _) -> pretty ("---" :: Text)
  (EdgeE, _) -> pretty ("--o" :: Text)
  (EdgeF, _) -> pretty ("--x" :: Text)
  (EdgeG, Just label) -> pretty ("--" :: Text) <> prEdgeLabel label <> pretty ("---" :: Text)
  (EdgeH, Just label) -> pretty ("--" :: Text) <> prEdgeLabel label <> pretty ("--" :: Text)
  (EdgeI, _) -> pretty ("==>" :: Text)
  (EdgeJ, Just label) -> pretty ("==" :: Text) <> prEdgeLabel label <> pretty ("==>" :: Text)
  (EdgeK, _) -> pretty ("o--o" :: Text)
  (EdgeL, _) -> pretty ("x--x" :: Text)
  (EdgeM, _) -> pretty ("<-->" :: Text)
  (EdgeN, Just label) -> pretty ("-." :: Text) <> prEdgeLabel label <> pretty ("->" :: Text)
  _ -> pretty ("" :: Text)

prSubFlowChartGraph :: SubFlowChartGraph -> [Doc ann]
prSubFlowChartGraph subFlowChartGraph = do
  (maybeEdge, leftNode, rightNode) <- edgeList subFlowChartGraph
  case maybeEdge of
    Just edge -> return (prNode leftNode <+> prEdge edge <+> prNode rightNode)
    Nothing -> return $ pretty ("" :: Text)

prOrientation :: Orientation -> Doc ann
prOrientation orientation = case orientation of
  TB -> pretty ("flowchart TB" :: Text)
  TD -> pretty ("flowchart TD" :: Text)
  BT -> pretty ("flowchart BT" :: Text)
  RL -> pretty ("flowchart RL" :: Text)
  LR -> pretty ("flowchart LR" :: Text)

prFlowChartGraph :: FlowChartGraph -> Doc ann
prFlowChartGraph (FlowChartGraph flowChartOrientation flowChartGraph) = vsep [nest 4 (vsep ([prOrientation flowChartOrientation] ++ (prSubFlowChartGraph flowChartGraph)))]

module PrettyPrinter.FlowChart where

import Diagram.FlowChart
import Prettyprinter

prNode :: Node -> Doc ann
prNode (Node nodeId nodeShape nodeLabel) = prNodeId nodeId <> prShape (nodeShape, nodeLabel)

prShape :: (Maybe Shape, Maybe NodeLabel) -> Doc ann
prShape (nodeShape,nodeLabel) = case (nodeShape, nodeLabel) of
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
                                                 
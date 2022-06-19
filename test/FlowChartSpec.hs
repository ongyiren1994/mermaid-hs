{-# LANGUAGE QuasiQuotes #-}

module FlowChartSpec
  ( spec,
  )
where

import Algebra.Graph.Labelled as LG (edgeList, vertexList)
import Control.Lens.Operators as Lens ((%~))
import Control.Lens.Tuple
import Diagram
import Diagram.FlowChart
import NeatInterpolation
import Test.Hspec
import Text.Megaparsec

shouldHaveEdges :: Maybe FlowChartGraph -> [(Edge, Node, Node)] -> Expectation
shouldHaveEdges flowChartGraph edges = fmap (edgeList . _flowChartGraph) flowChartGraph `shouldBe` Just (fmap (& _1 %~ Just) edges)

infix 1 `shouldHaveEdges`

shouldHaveNodes :: Maybe FlowChartGraph -> [Node] -> Expectation
shouldHaveNodes flowChartGraph nodes = fmap (vertexList . _flowChartGraph) flowChartGraph `shouldBe` Just nodes

infix 1 `shouldHaveNodes`

spec :: Spec
spec = do
  describe "FlowChart" $ do
    it "nodes of single node" $ do
      let flowChartGraph =
            [text|
              flowchart TD
                id
            |]
      parseMaybe pFlowChartGraph flowChartGraph
        `shouldHaveNodes` ["id"]
    it "edges of single node" $ do
      let flowChartGraph =
            [text|
              flowchart TD
                id
            |]
      parseMaybe pFlowChartGraph flowChartGraph
        `shouldHaveEdges` []
    it "nodes of single edge with two nodes" $ do
      let flowChartGraph =
            [text|
              flowchart TD
                start --> stop
            |]
      parseMaybe pFlowChartGraph flowChartGraph
        `shouldHaveNodes` [ "start",
                            "stop"
                          ]
    it "edges of single edge with two nodes" $ do
      let flowChartGraph =
            [text|
              flowchart TD
                start --> stop
            |]
      parseMaybe pFlowChartGraph flowChartGraph
        `shouldHaveEdges` [(Edge EdgeB Nothing, "start", "stop")]
    it "nodes of single edge with two times two isolated nodes" $ do
      let flowChartGraph =
            [text|
              flowchart TD
                A & B--> C & D
            |]
      parseMaybe pFlowChartGraph flowChartGraph
        `shouldHaveNodes` [ "A",
                            "B",
                            "C",
                            "D"
                          ]
    it "edges of single edge with two times two isolated nodes" $ do
      let flowChartGraph =
            [text|
              flowchart TD
                A & B--> C & D
            |]
      parseMaybe pFlowChartGraph flowChartGraph
        `shouldHaveEdges` [ (Edge EdgeB Nothing, "A", "C"),
                            (Edge EdgeB Nothing, "A", "D"),
                            (Edge EdgeB Nothing, "B", "C"),
                            (Edge EdgeB Nothing, "B", "D")
                          ]

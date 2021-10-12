{-# LANGUAGE QuasiQuotes #-}

module FlowChartSpec
  ( spec,
  )
where

import Algebra.Graph.Labelled as LG (edgeList, vertexList)
import Control.Lens.Operators as Lens ((%~))
import Control.Lens.Tuple
import Diagram
import FlowChart
import NeatInterpolation
import Test.Hspec
import Text.Megaparsec

shouldHaveEdges :: Maybe Diagram -> [(Edge, Node, Node)] -> Expectation
shouldHaveEdges diagram edges = fmap (edgeList . _graph) diagram `shouldBe` Just (fmap (& _1 %~ Just) edges)

infix 1 `shouldHaveEdges`

shouldHaveNodes :: Maybe Diagram -> [Node] -> Expectation
shouldHaveNodes diagram nodes = fmap (vertexList . _graph) diagram `shouldBe` Just nodes

infix 1 `shouldHaveNodes`

spec :: Spec
spec = do
  describe "FlowChart" $ do
    it "nodes of single node" $ do
      let diagram =
            [text|
              flowchart TD
                id
            |]
      parseMaybe pDiagram diagram
        `shouldHaveNodes` ["id"]
    it "edges of single node" $ do
      let diagram =
            [text|
              flowchart TD
                id
            |]
      parseMaybe pDiagram diagram
        `shouldHaveEdges` []
    it "nodes of single edge with two nodes" $ do
      let diagram =
            [text|
              flowchart TD
                start --> stop
            |]
      parseMaybe pDiagram diagram
        `shouldHaveNodes` [ "start",
                            "stop"
                          ]
    it "edges of single edge with two nodes" $ do
      let diagram =
            [text|
              flowchart TD
                start --> stop
            |]
      parseMaybe pDiagram diagram
        `shouldHaveEdges` [("-->", "start", "stop")]
    it "nodes of single edge with two times two isolated nodes" $ do
      let diagram =
            [text|
              flowchart TD
                A & B--> C & D
            |]
      parseMaybe pDiagram diagram
        `shouldHaveNodes` [ "A",
                            "B",
                            "C",
                            "D"
                          ]
    it "edges of single edge with two times two isolated nodes" $ do
      let diagram =
            [text|
              flowchart TD
                A & B--> C & D
            |]
      parseMaybe pDiagram diagram
        `shouldHaveEdges` [ ("-->", "A", "C"),
                            ("-->", "A", "D"),
                            ("-->", "B", "C"),
                            ("-->", "B", "D")
                          ]
{-# LANGUAGE DeriveGeneric #-}

module GanttChart where

import Data.List.Split
import Parser
import Text.Megaparsec as M
import Text.Megaparsec.Char (alphaNumChar, char, string)

data GExpr
  = GExprDateFormat DateFormat
  | GExprGanttChartTitle GanttChartTitle
  | GExprAxisFormat AxisFormat
  | GExprSectionTitle SectionTitle
  | GExprTask Task
  | GExprEmptyLine
  deriving (Eq, Show, Generic)

data GanttChartGraph = GanttChartGraph
  { _ganttChartTitle :: GanttChartTitle,
    _dateFormat :: DateFormat,
    _axisFormat :: AxisFormat,
    _section :: [Section]
  }
  deriving (Eq, Show, Generic)

data Section = Section
  { _sectionTitle :: SectionTitle,
    _tasks :: [Task]
  }
  deriving (Eq, Show, Generic)

data Task = Task
  { _taskName :: TaskName,
    _taskState :: TaskState
  }
  deriving (Eq, Show, Generic)

pTaskState :: Parser TaskState
pTaskState =
  choice
    [ Done <$ string "done",
      Active <$ string "active",
      Crit <$ string "crit",
      After <$ string "after",
      OtherState <$ M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%')
    ]

newtype GanttChartTitle = GanttChartTitle {unGanttChartTitle :: Text} deriving (Eq, Show, Generic)

newtype DateFormat = DateFormat {unDateFormat :: Text} deriving (Eq, Show, Generic)

newtype AxisFormat = AxisFormat {unAxisFormat :: Text} deriving (Eq, Show, Generic)

newtype SectionTitle = SectionTitle {unSectionTile :: Text} deriving (Eq, Show, Generic)

newtype TaskName = TaskName {unTaskName :: Text} deriving (Eq, Show, Generic)

data TaskState = Done | Active | Crit | After | OtherState deriving (Eq, Show, Generic)

instance IsString GanttChartGraph where
  fromString s = GanttChartGraph (fromString s) "" "" []

instance IsString Section where
  fromString s = Section (fromString s) []

instance IsString GanttChartTitle where
  fromString s = GanttChartTitle (fromString s)

instance IsString DateFormat where
  fromString s = DateFormat (fromString s)

instance IsString AxisFormat where
  fromString s = AxisFormat (fromString s)

instance IsString SectionTitle where
  fromString s = SectionTitle (fromString s)

instance IsString Task where
  fromString s = Task (fromString s) Done

instance IsString TaskName where
  fromString s = TaskName (fromString s)

pGanttChartTitle :: Parser GExpr
pGanttChartTitle = do
  void $ lexeme $ string "title"
  ganttChartTitle <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%'))
  return $ GExprGanttChartTitle ganttChartTitle

pDateFormat :: Parser GExpr
pDateFormat = do
  void $ lexeme $ string "dateFormat"
  dateFormat <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%'))
  return $ GExprDateFormat dateFormat

pAxisFormat :: Parser GExpr
pAxisFormat = do
  void $ lexeme $ string "axisFormat"
  axisFormat <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%'))
  return $ GExprAxisFormat axisFormat

pSectionTitle :: Parser GExpr
pSectionTitle = do
  void $ lexeme $ string "section"
  sectionTitle <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%'))
  return $ GExprSectionTitle sectionTitle

pTask :: Parser GExpr
pTask = do
  taskName <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char '%'))
  void $ lexeme $ string ":"
  taskState <- lexeme pTaskState
  void $ lexeme $ M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%')
  return $ GExprTask $ Task taskName taskState

pGanttChart :: Parser GExpr
pGanttChart =
  pGanttChartTitle <|> pDateFormat <|> pAxisFormat <|> pSectionTitle <|> pTask

gExprToGanttChartGraph :: [GExpr] -> GanttChartGraph
gExprToGanttChartGraph gExprs =
  case gExprs of
    GExprGanttChartTitle a : GExprDateFormat b : GExprAxisFormat c : d -> GanttChartGraph a b c $ gExprToSection <$> splitGExprTask d
    _ -> error "Invalid"

splitGExprTask :: [GExpr] -> [[GExpr]]
splitGExprTask = split (keepDelimsL . dropInitBlank $ whenElt isGExprSectionTitle)

isGExprSectionTitle :: GExpr -> Bool
isGExprSectionTitle gExpr = case gExpr of
  GExprSectionTitle _ -> True
  _ -> False

gExprToSection :: [GExpr] -> Section
gExprToSection gExprs = do
  case gExprs of
    GExprSectionTitle a : b -> Section a $ fmap gExprToTask b
    _ -> error "Invalid"

gExprToTask :: GExpr -> Task
gExprToTask (GExprTask a) = a
gExprToTask _ = error "Invalid"
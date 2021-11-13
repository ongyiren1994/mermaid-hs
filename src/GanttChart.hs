{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GanttChart where

import Control.Lens.TH (makeLenses)
import Parser
import Text.Megaparsec as M
import Text.Megaparsec.Char (alphaNumChar, char, string)

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
    _taskState :: Maybe TaskState
  }
  deriving (Eq, Show, Generic)

newtype GanttChartTitle = GanttChartTitle {unGanttChartTitle :: Text} deriving (Eq, Show, Generic)

newtype DateFormat = DateFormat {unDateFormat :: Text} deriving (Eq, Show, Generic)

newtype AxisFormat = AxisFormat {unAxisFormat :: Text} deriving (Eq, Show, Generic)

newtype SectionTitle = SectionTitle {unSectionTile :: Text} deriving (Eq, Show, Generic)

newtype TaskName = TaskName {unTaskName :: Text} deriving (Eq, Show, Generic)

data TaskState = Done | Active | Crit | After deriving (Eq, Show, Generic)

makeLenses ''GanttChartGraph

makeLenses ''GanttChartTitle

makeLenses ''DateFormat

makeLenses ''AxisFormat

makeLenses ''Section

makeLenses ''SectionTitle

makeLenses ''Task

makeLenses ''TaskName

makeLenses ''TaskState

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
  fromString s = Task (fromString s) Nothing

instance IsString TaskName where
  fromString s = TaskName (fromString s)

pGanttChartTitle :: Parser GanttChartTitle
pGanttChartTitle = do
  void $ lexeme $ string "title"
  lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%'))

pDateFormat :: Parser DateFormat
pDateFormat = do
  void $ lexeme $ string "dateFormat"
  lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%'))

pAxisFormat :: Parser AxisFormat
pAxisFormat = do
  void $ lexeme $ string "axisFormat"
  lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%'))

pSections :: Pos -> Parser [Section]
pSections ref = M.many $ pSection ref

pSection :: Pos -> Parser Section
pSection ref = do
  void $ pCheckIndent ref
  sectionTitle <- pSectionTitle
  tasks <- pTasks ref
  return $ Section sectionTitle tasks

pTasks :: Pos -> Parser [Task]
pTasks ref = M.many $
  try $ do
    void $ pCheckIndent ref
    pTask

pSectionTitle :: Parser SectionTitle
pSectionTitle = do
  void $ lexeme $ string "section"
  lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%'))

pTask :: Parser Task
pTask = do
  taskName <- lexeme (fromString <$> M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char '%'))
  void $ lexeme $ string ":"
  taskState <- lexeme $ optional pTaskState
  void $ lexeme $ M.some (alphaNumChar <|> char ' ' <|> char '?' <|> char '!' <|> char '\'' <|> char ',' <|> char ':' <|> char '%')
  return $ Task taskName taskState

pTaskState :: Parser TaskState
pTaskState =
  choice
    [ Done <$ string "done",
      Active <$ string "active",
      Crit <$ string "crit",
      After <$ string "after"
    ]
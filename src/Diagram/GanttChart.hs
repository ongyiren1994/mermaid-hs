{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Diagram.GanttChart where

import Control.Lens.TH (makeLenses)
import Parser
import Text.Megaparsec as M
import Text.Megaparsec.Char (char, string)

data GanttChartGraph = GanttChartGraph
  { _ganttChartTitle :: GanttChartTitle,
    _ganttChartDateFormat :: DateFormat,
    _ganttChartAxisFormat :: AxisFormat,
    _ganttChartSection :: [Section]
  }
  deriving (Eq, Show, Generic)

data Section = Section
  { _sectionTitle :: SectionTitle,
    _sectionTasks :: [Task]
  }
  deriving (Eq, Show, Generic)

data Task = Task
  { _taskName :: TaskName,
    _taskState :: Maybe TaskState
  }
  deriving (Eq, Show, Generic)

newtype GanttChartTitle = GanttChartTitle {unGanttChartTitle :: Text} deriving (Eq, Show, Generic, IsString)

newtype DateFormat = DateFormat {unDateFormat :: Text} deriving (Eq, Show, Generic, IsString)

newtype AxisFormat = AxisFormat {unAxisFormat :: Text} deriving (Eq, Show, Generic, IsString)

newtype SectionTitle = SectionTitle {unSectionTile :: Text} deriving (Eq, Show, Generic, IsString)

newtype TaskName = TaskName {unTaskName :: Text} deriving (Eq, Show, Generic, IsString)

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

instance IsString Task where
  fromString s = Task (fromString s) Nothing

pGanttChartTitle :: Parser GanttChartTitle
pGanttChartTitle = lexeme $ string "title" >> lexeme (fromString <$> M.some (pText <|> char ':'))

pDateFormat :: Parser DateFormat
pDateFormat = lexeme $ string "dateFormat" >> lexeme (fromString <$> M.some (pText <|> char ':'))

pAxisFormat :: Parser AxisFormat
pAxisFormat = lexeme $ string "axisFormat" >> lexeme (fromString <$> M.some (pText <|> char ':'))

pSections :: Pos -> Parser [Section]
pSections ref = M.many $ pSection ref

pSection :: Pos -> Parser Section
pSection ref = do
  void $ pCheckIndent ref
  title <- pSectionTitle
  tasks <- pTasks ref
  return $ Section title tasks

pTasks :: Pos -> Parser [Task]
pTasks ref = M.many $ try $ pCheckIndent ref >> pTask

pSectionTitle :: Parser SectionTitle
pSectionTitle = lexeme $ string "section" >> lexeme (fromString <$> M.some (pText <|> char ':'))

pTask :: Parser Task
pTask = do
  name <- lexeme (fromString <$> M.some pText)
  void $ lexeme $ string ":"
  state' <- lexeme $ optional pTaskState
  void $ lexeme $ M.some (pText <|> char ':')
  return $ Task name state'

pTaskState :: Parser TaskState
pTaskState =
  choice
    [ Done <$ string "done",
      Active <$ string "active",
      Crit <$ string "crit",
      After <$ string "after"
    ]

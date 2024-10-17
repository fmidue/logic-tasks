{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Syntax.IllegalDnfs where


import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  english,
  german,
  Rated,
  multipleChoice,
  ArticleToUse (DefiniteArticle),
  translations,
  multipleChoiceSyntax,
  )
import Data.Map as Map (fromAscList)
import LogicTasks.Helpers
import Tasks.LegalCNF.Config(LegalCNFConfig(..), LegalCNFInst(..), checkLegalCNFConfig)
import qualified LogicTasks.Syntax.IllegalCnfs




description :: OutputCapable m => LegalCNFInst -> LangM m
description = LogicTasks.Syntax.IllegalCnfs.descriptionTemplate $ translations $ do
  german "disjunktiver Normalform (DNF)"
  english "disjunctive normal form (dnf)"



verifyInst :: OutputCapable m => LegalCNFInst -> LangM m
verifyInst = LogicTasks.Syntax.IllegalCnfs.verifyInst



verifyConfig :: OutputCapable m => LegalCNFConfig -> LangM m
verifyConfig = LogicTasks.Syntax.IllegalCnfs.verifyConfig



start :: [Int]
start = []



partialGrade :: OutputCapable m => LegalCNFInst -> [Int] -> LangM m
partialGrade = LogicTasks.Syntax.IllegalCnfs.partialGrade


completeGrade :: OutputCapable m => LegalCNFInst -> [Int] -> Rated m
completeGrade = LogicTasks.Syntax.IllegalCnfs.completeGrade

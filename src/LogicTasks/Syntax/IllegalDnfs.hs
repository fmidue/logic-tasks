{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
module LogicTasks.Syntax.IllegalDnfs where


import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  english,
  german,
  Rated,
  translations,
  )
import Tasks.LegalNormalForm.Config(LegalNormalFormConfig(..), LegalNormalFormInst(..))
import qualified LogicTasks.Syntax.IllegalCnfs as IllegalCnfs
import Control.Applicative (Alternative)




description :: OutputCapable m => Bool -> LegalNormalFormInst -> LangM m
description = IllegalCnfs.descriptionTemplate $ translations $ do
  german "disjunktiver Normalform (DNF)"
  english "disjunctive normal form (dnf)"



verifyInst :: OutputCapable m => LegalNormalFormInst -> LangM m
verifyInst = IllegalCnfs.verifyInst



verifyConfig :: OutputCapable m => LegalNormalFormConfig -> LangM m
verifyConfig = IllegalCnfs.verifyConfig



start :: [Int]
start = IllegalCnfs.start



partialGrade :: OutputCapable m => LegalNormalFormInst -> [Int] -> LangM m
partialGrade = IllegalCnfs.partialGrade


completeGrade :: (OutputCapable m, Alternative m, Monad m) => LegalNormalFormInst -> [Int] -> Rated m
completeGrade = IllegalCnfs.completeGrade

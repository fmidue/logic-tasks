{-# language RecordWildCards #-}

module LogicTasks.Semantics.Prolog where


import Control.Monad.Output (LangM, OutputMonad (..), english, german, translate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (difference, member, toList, union)
import Data.Tuple (swap)
import Test.QuickCheck (Gen)

import Config (PrologConfig(..), PrologInst(..))
import Formula.Types (Clause, Literal(..), PrologLiteral(..), PrologClause, literals, pliterals, opposite)
import Formula.Util (flipPol, isEmptyClause, isPositive, mkPrologClause, transformProlog)
import Formula.Resolution (resolvable, resolve)
import LogicTasks.Semantics.Step (genResStepClause)
import Util(prevent, preventWithHint)




genPrologInst :: PrologConfig -> Gen PrologInst
genPrologInst PrologConfig{..} = do
    (clause, resolveLit, literals1) <- genResStepClause minClauseLength maxClauseLength usedLiterals
    let
      termAddedClause1 = mkPrologClause $ map remap (resolveLit : literals1)
      termAddedClause2 = mkPrologClause $ map remap (opposite resolveLit : literals clause)
    pure $ PrologInst termAddedClause1 termAddedClause2 extraText
  where
    mapping = zip usedPredicates ['A'..'Z']
    usedLiterals = map snd mapping
    reverseMapping = map swap mapping
    remap l = if isPositive l then predicate else flipPol predicate
      where
        predicate = fromJust (lookup (letter l) reverseMapping)



description :: OutputMonad m => PrologInst -> LangM m
description PrologInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die zwei folgenden Klauseln:"
      english "Consider the two following clauses:"
    indent $ code $ show literals1
    indent $ code $ show literals2

  paragraph $ translate $ do
    german "Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
    english "Resolve the clauses and give the resulting resolvent."

  paragraph $ translate $ do
    german "Geben Sie das in dem Resolutionsschritt genutzte Literal und das Ergebnis in der folgenden Tupelform an: (Literal, Resolvente)."
    english "Provide the literal used for the step and the resolvent in the following tuple form: (literal, resolvent)."

  paragraph $ translate $ do
    german "Die leere Klausel kann durch geschweifte Klammern '{ }' dargestellt werden."
    english "The empty clause can be denoted by curly braces '{ }'."

  paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch mit den Klauseln a(x) und not(a(x)) könnte beispielsweise so aussehen:"
      english "A valid solution with the clauses a(x) and not(a(x)) could look like this:"
    code "(a(x), { })"

  paragraph $ text (fromMaybe "" addText)



verifyStatic :: OutputMonad m => PrologInst -> LangM m
verifyStatic PrologInst{..}
    | any isEmptyClause [clause1, clause2] =
        refuse $ indent $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | not $ resolvable clause1 clause2 =
        refuse $ indent $ translate $ do
          german "Die Klauseln sind nicht resolvierbar."
          english "The clauses are not resolvable."

    | otherwise = pure()
  where
    (clause1, clause2, _) = transform (literals1, literals2)



verifyQuiz :: OutputMonad m => PrologConfig -> LangM m
verifyQuiz PrologConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        refuse $ indent $ translate $ do
          german "Mindestens eines der 'length'-Parameter ist negativ."
          english "At least one length parameter is negative."

    | minClauseLength > maxClauseLength =
        refuse $ indent $ translate $ do
          german "Die untere Grenze der Klausellänge ist höher als die obere."
          english "The minimum clause length is greater than the maximum clause length."

    | length usedPredicates < minClauseLength =
        refuse $ indent $ translate $ do
          german "Zu wenige Literale für diese Klausellänge."
          english "There are not enough literals available for this clause length."

    | null usedPredicates =
        refuse $ indent $ translate $ do
          german "Es wurden keine Literale angegeben."
          english "You did not specify which literals should be used."

    | otherwise = pure()



start :: (PrologLiteral, PrologClause)
start = (PrologLiteral True " " [], mkPrologClause [])



partialGrade :: OutputMonad m => PrologInst -> (PrologLiteral, PrologClause) -> LangM m
partialGrade PrologInst{..} sol = do
  prevent (not (fst sol `member` availLits)) $
    translate $ do
      german "Gewähltes Literal kommt in den Klauseln vor?"
      english "Chosen literal is contained in any of the clauses?"

  preventWithHint (not $ null extra)
    (translate $ do
       german "Resolvente besteht aus bekannten Literalen?"
       english "Resolvent contains only known literals?"
    )
    (paragraph $ do
      translate $ do
        german "In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
        english "The resolvent contains unknown literals. These are incorrect:"
      itemizeM $ map (text . show) extra
    )
  where
     availLits = pliterals literals1 `union` pliterals literals2
     solLits = pliterals $ snd sol
     extra = toList $ solLits `difference` availLits



completeGrade :: OutputMonad m => PrologInst -> (PrologLiteral, PrologClause) -> LangM m
completeGrade PrologInst{..} sol =
    case resolve clause1 clause2 transSol1 of
        Nothing -> refuse $ indent $ translate $ do
                     german "Mit diesem Literal kann kein Schritt durchgeführt werden!"
                     english "This literal can not be used for a resolution step!"

        Just solClause -> if solClause == transSol2
                            then pure()
                            else refuse $ indent $ translate $ do
                                   german "Resolvente ist nicht korrekt."
                                   english "Resolvent is not correct."
  where
    (clause1, clause2, mapping) = transform (literals1, literals2)
    transSol1 = fromJust $ lookup (fst sol) mapping
    transSol2 = transformProlog (snd sol) mapping



transform :: (PrologClause,PrologClause) -> (Clause,Clause,[(PrologLiteral,Literal)])
transform (pc1,pc2) = (clause1, clause2, applyPol)
  where
    allPreds = toList (pliterals pc1 `union` pliterals pc2)
    noDups = map (\(PrologLiteral _ n f) -> PrologLiteral True n f) allPreds
    mapping = zip noDups ['A'..'Z']

    polLookup p = (p,lit)
      where lit = case lookup p mapping of
                    Just l1  -> Literal l1
                    Nothing  -> case lookup (flipPol p) mapping of
                                  Just l2 -> Not l2
                                  Nothing -> error "each literal should have a mapping."

    applyPol = map polLookup allPreds
    clause1 = transformProlog pc1 applyPol
    clause2 = transformProlog pc2 applyPol



revertMapping :: [Literal] -> [(PrologLiteral,Literal)] -> [PrologLiteral]
revertMapping ls mapping = map fromJust getPreds
  where
    reverseM = map swap mapping
    getPreds = map (`lookup` reverseM) ls


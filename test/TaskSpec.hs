module TaskSpec (spec) where

import Test.QuickCheck
import Data.Text
import Test.Hspec
import FindLiarTask(taskData,makeHintsAndFormula)
import FindLiar2(makeHintsAndFormula)
import Trees.Types (BinOp(..), SynTree(..))

spec :: Spec
spec = do
       describe "FindLiarTask" $ do
           it "print taskData: formula, hints" $ do
             d <- generate taskData
             d' <- (\(x,y) -> return (x, fmap unpack y)) $ FindLiarTask.makeHintsAndFormula d
             sequence_ $ fmap print (fst d') ++ fmap print (snd d')
           it "flip needed" $ do
             fst (FindLiarTask.makeHintsAndFormula (('C', True), ('B', False), ('A', False), True)) `shouldBe`
               [Binary Equi (Leaf 'C') (Not (Leaf 'B')),
                Binary Equi (Leaf 'B') (Leaf 'A'),
                Binary Equi (Leaf 'A') (Binary And (Leaf 'B') (Leaf 'C'))]
           it "no flip needed" $ do
             fst (FindLiarTask.makeHintsAndFormula (('A', True), ('B', False), ('C', False), True)) `shouldBe`
               [Binary Equi (Leaf 'A') (Not (Leaf 'B')),
                Binary Equi (Leaf 'B') (Leaf 'C'),
                Binary Equi (Leaf 'C') (Binary And (Leaf 'A') (Leaf 'B'))]

       describe "LiarTask2" $ do
         it "flip needed" $ do
           fst (FindLiar2.makeHintsAndFormula (('C', True), ('A', False), ('B', False))) `shouldBe`
             [Binary Equi (Leaf 'C') (Binary Or (Not (Leaf 'A')) (Leaf 'B')),
              Binary Equi (Leaf 'A') (Binary And (Leaf 'B') (Not (Leaf 'C'))),
              Binary Equi (Leaf 'B') (Binary Or (Leaf 'A') (Not (Leaf 'C')))]
         it "no flip needed" $ do
           fst (FindLiar2.makeHintsAndFormula (('A', True), ('B', False), ('C', False))) `shouldBe`
             [Binary Equi (Leaf 'A') (Binary Or (Not (Leaf 'B')) (Leaf 'C')),
              Binary Equi (Leaf 'B') (Binary And (Not (Leaf 'A')) (Leaf 'C')),
              Binary Equi (Leaf 'C') (Binary Or (Not (Leaf 'A')) (Leaf 'B'))]

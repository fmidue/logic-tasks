module TaskSpec (spec) where

import Test.QuickCheck
import Data.Text
import Test.Hspec
import FindLiarTask(taskData,makeHintsAndFormula)
import Trees.Types (BinOp(..), SynTree(..))

spec :: Spec
spec = do
       describe "Tests" $ do
           it "print taskData: formula, hints" $ do
             d <- generate taskData
             d' <- (\(x,y) -> return (x, fmap unpack y)) $ makeHintsAndFormula d
             sequence_ $ fmap print (fst d') ++ fmap print (snd d')
           it "no flip needed" $ do
             fst (makeHintsAndFormula (('C', True), ('B', False), ('A', False), True)) `shouldBe`
               [Binary Equi (Leaf 'C') (Not (Leaf 'B')),
                Binary Equi (Leaf 'B') (Leaf 'A'),
                Binary Equi (Leaf 'A') (Binary And (Leaf 'B') (Leaf 'C'))]
           it "flip needed" $ do
             fst (makeHintsAndFormula (('A', True), ('B', False), ('C', False), True)) `shouldBe`
               [Binary Equi (Leaf 'A') (Not (Leaf 'B')),
                Binary Equi (Leaf 'B') (Leaf 'C'),
                Binary Equi (Leaf 'C') (Binary And (Leaf 'A') (Leaf 'B'))]


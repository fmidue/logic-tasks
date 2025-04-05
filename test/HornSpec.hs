module HornSpec (spec) where

import Test.Hspec
import Horn (startAlgorithm)
import Trees.Types (BinOp(..), SynTree(..))

spec :: Spec
spec = do
  describe "Markierungsalgorithmus" $ do
    it "1->A->B and A->0" $ do
      startAlgorithm (foldr1 (Binary And)
        [ Binary Impl (Leaf 'A') (Leaf 'B')
        , Binary Impl (Leaf '1') (Leaf 'A')
        , Binary Impl (Leaf 'A') (Leaf '0')
        ]) `shouldBe`
        ([(1,['A']),(2,['B'])], False, []) -- oder sollte B nicht erst markiert werden?
    it "1->A->B and (A&B)->0" $ do
      startAlgorithm (foldr1 (Binary And)
        [ Binary Impl (Leaf 'A') (Leaf 'B')
        , Binary Impl (Leaf '1') (Leaf 'A')
        , Binary Impl (Binary And (Leaf 'A') (Leaf 'B')) (Leaf '0')
        ]) `shouldBe`
        ([(1,['A']),(2,['B'])], False, [])
    it "1->A->B->C and (A&B&C)->0" $ do
      startAlgorithm (foldr1 (Binary And)
        [ Binary Impl (Leaf 'A') (Leaf 'B')
        , Binary Impl (Leaf '1') (Leaf 'A')
        , Binary Impl (Leaf 'B') (Leaf 'C')
        , Binary Impl (Binary And (Binary And (Leaf 'A') (Leaf 'B')) (Leaf 'C')) (Leaf '0')
        ]) `shouldBe`
        ([(1,['A']),(2,['B']),(3,['C'])], False, [])
    it "1->A->B and (B&C)->0" $ do
      startAlgorithm (foldr1 (Binary And)
        [ Binary Impl (Leaf 'A') (Leaf 'B')
        , Binary Impl (Leaf '1') (Leaf 'A')
        , Binary Impl (Binary And (Leaf 'B') (Leaf 'C')) (Leaf '0')
        ]) `shouldBe`
        ([(1,['A']),(2,['B'])], True, [('A', True),('B', True),('C', False)])

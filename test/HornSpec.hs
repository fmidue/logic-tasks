module HornSpec (spec) where

import Test.Hspec
import Horn
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

  describe "Check Different Solution" $ do
    it "correct order, using a shortcut" $ do
      newSolution (foldr1 (Binary And) v3) [(1,"BA"),(2,"D"),(3,"E")]
      `shouldBe`
      ([(1,"BA"),(2,"D"),(3,"E")],False,[])

    it "different but correct order of marks" $ do
      newSolution (foldr1 (Binary And) v3) [(1,"BA"),(2,"D"),(3,"C"),(4,"E")]
      `shouldBe`
      ([(1,"BA"),(2,"D"),(3,"C"),(4,"E")],False,[])

    it "illegal marking of E without marking D" $ do
      newSolution (foldr1 (Binary And) v3) [(1,"BA"),(2,"C"),(3,"E")]
      `shouldBe`
      ([(1,"AB"),(2,"C"),(3,"D"),(4,"E")],False,[])


    it "correct order, missing marks" $ do
      newSolution (foldr1 (Binary And) v4) [(1,"BA"),(2,"D"),(3,"E")]
      `shouldBe`
      ([(1,"BA"),(2,"D"),(3,"E"),(4,"C")],True,[('B',True),('A',True),('D',True),('E',True),('C',True),('F',False)])

    it "different but correct order of marks" $ do
      newSolution (foldr1 (Binary And) v4) [(1,"BA"),(2,"D"),(3,"E"),(4,"C")]
      `shouldBe`
      ([(1,"BA"),(2,"D"),(3,"E"),(4,"C")],True,[('B',True),('A',True),('D',True),('E',True),('C',True),('F',False)])

    it "illegal marking of E without marking D" $ do
      newSolution (foldr1 (Binary And) v4) [(1,"BA"),(2,"C"),(3,"E")]
      `shouldBe`
      ([(1,"AB"),(2,"C"),(3,"D"),(4,"E")],True,[('A',True),('B',True),('C',True),('D',True),('E',True),('F',False)])

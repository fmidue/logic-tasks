module HornSpec (spec) where

import Test.Hspec
import Test.QuickCheck (generate)
import Horn
import Trees.Types (BinOp(..), SynTree(..))
import Trees.Print


spec :: Spec
spec = do
  describe "HornGenerator Examples" $ do
    it "v1 with 2 extra" $ do
      x <- generate $ makeHornFormula v1 2
      print $ simplestDisplay x

    it "v2 with 2 extra" $ do
      y <- generate $ makeHornFormula v2 2
      print $ simplestDisplay y


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
    it "C->A->B and (B&C)->0 -- no facts" $ do
      startAlgorithm (foldr1 (Binary And)
        [ Binary Impl (Leaf 'A') (Leaf 'B')
        , Binary Impl (Leaf 'C') (Leaf 'A')
        , Binary Impl (Binary And (Leaf 'B') (Leaf 'C')) (Leaf '0')
        ]) `shouldBe`
        ([], True, [('A', False),('B', False),('C', False)])


  describe "Check Different Solution" $ do
    it "empty protocol" $ do
      newSolution (foldr1 (Binary And) v3) []
      `shouldBe`
       ([(1,"AB"),(2,"C"),(3,"D"),(4,"E")],False,[])

    it "false first step" $ do
      newSolution (foldr1 (Binary And) v3) [(1,"B"),(2,"A"),(3,"D"),(4,"E")]
      `shouldBe`
       ([(1,"AB"),(2,"C"),(3,"D"),(4,"E")],False,[])

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

    it "so far correct, but incomplete marking" $ do
      newSolution (foldr1 (Binary And) v3) [(1,"AB"),(2,"C")]
      `shouldBe`
      ([(1,"AB"),(2,"C"),(3,"D"),(4,"E")],False,[])

    it "multiple wrong marks" $ do
      newSolution (foldr1 (Binary And) v3) [(1,"ABX"),(2,"Y")]
      `shouldBe`
      ([(1,"AB"),(2,"C"),(3,"D"),(4,"E")],False,[])

    it "correct order, missing marks" $ do
      newSolution (foldr1 (Binary And) v4) [(1,"BA"),(2,"D"),(3,"E")]
      `shouldBe`
      ([(1,"BA"),(2,"D"),(3,"E"),(4,"C")],True,[('A',True),('B',True),('C',True),('D',True),('E',True),('F',False)])

    it "different but correct order of marks" $ do
      newSolution (foldr1 (Binary And) v4) [(1,"BA"),(2,"D"),(3,"E"),(4,"C")]
      `shouldBe`
      ([(1,"BA"),(2,"D"),(3,"E"),(4,"C")],True,[('A',True),('B',True),('C',True),('D',True),('E',True),('F',False)])

    it "illegal marking of E without marking D" $ do
      newSolution (foldr1 (Binary And) v4) [(1,"BA"),(2,"C"),(3,"E")]
      `shouldBe`
      ([(1,"AB"),(2,"C"),(3,"D"),(4,"E")],True,[('A',True),('B',True),('C',True),('D',True),('E',True),('F',False)])


  describe "isHornFormulaI" $ do
    it "1 -> A" $ do
      let formula = Binary Impl (Leaf '1') (Leaf 'A')
      isHornFormulaI formula `shouldBe` True

    it "((A and B) and C) -> D is a valid Horn clause" $ do
      let formula = Binary Impl
                      (Binary And (Binary And (Leaf 'A') (Leaf 'B')) (Leaf 'C'))
                      (Leaf 'D')
      isHornFormulaI formula `shouldBe` True

    it "(A and (B and C)) -> D is a valid Horn clause" $ do
      let formula = Binary Impl
                      (Binary And (Leaf 'A') (Binary And (Leaf 'B') (Leaf 'C')))
                      (Leaf 'D')
      isHornFormulaI formula `shouldBe` True

    it "(A or B) -> C" $ do
      let formula = Binary Impl (Binary Or (Leaf 'A') (Leaf 'B')) (Leaf 'C')
      isHornFormulaI formula `shouldBe` False

    it "A -> (B and C)" $ do
      let formula = Binary Impl (Leaf 'A') (Binary And (Leaf 'B') (Leaf 'C'))
      isHornFormulaI formula `shouldBe` False

    it "A and B" $ do
      let formula = Binary And (Leaf 'A') (Leaf 'B')
      isHornFormulaI formula `shouldBe` False

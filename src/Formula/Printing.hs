{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wwarn=x-partial #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Formula.Printing
       ( showIndexedList
       ) where


import Config
import Formula.Types

import Data.List (intercalate, intersperse, nub)
import Data.Text.Lazy (pack)
import qualified Data.Set as Set (null)

import Text.PrettyPrint.Leijen.Text
import Data.Map (toList)
import Data.Maybe (isJust, fromJust)
import Trees.Print ()


myText :: String -> Doc
myText = text . pack




instance Pretty Number where
  pretty num = maybe empty pretty $ value num



instance Pretty StepAnswer where
  pretty a = maybe empty pretty $ step a


instance Pretty TruthValue where
  pretty tv = pretty $ truth tv


instance Pretty ResStep where
  pretty (Res (a,b,(c,d))) = tupled [litsOrNum a, litsOrNum b, withNumber]
    where
      litsOrNum = either prettyClause pretty
      curlyBracesList = encloseSep (char '{') (char '}') (char ',')
      prettyClause = curlyBracesList . map pretty . literals
      withNumber = prettyClause c <> if isJust d then myText (" = " ++ show (fromJust d))  else empty



instance Pretty Literal where
   pretty (Literal x) = char x
   pretty (Not x) = myText ['¬', x]



instance Pretty Clause where
    pretty clause = listShow $ literals clause
      where
        listShow [] = text "{ }"
        listShow [x] = pretty x
        listShow (x:xs) = hsep [pretty x, text "∨", listShow xs]



instance Pretty Con where
    pretty con = listShow $ literals con
      where
        listShow [] = empty
        listShow [x] = pretty x
        listShow (x:xs) = hsep [pretty x, text "∧", listShow xs]




instance Pretty Cnf where
    pretty cnf = listShow $ getClauses cnf
      where
        listShow [] = empty
        listShow [x] = singlePrint x
        listShow (x:xs) = hsep
                           [ singlePrint x
                           , text "∧"
                           , listShow xs
                           ]

        singlePrint x = if amount x == 1
                          then pretty x
                          else hcat [char '(', pretty x, char ')']



instance Pretty Dnf where
    pretty dnf = listShow $ getConjunctions dnf
      where
        listShow [] = empty
        listShow [x] = singlePrint x
        listShow (x:xs) = hsep
                           [ singlePrint x
                           , text "∨"
                           , listShow xs
                           ]

        singlePrint x = if amount x == 1
                          then pretty x
                          else hcat [char '(', pretty x, char ')']




instance Pretty Table where
  pretty t = myText (show t)



instance Pretty PrologLiteral where
    pretty (PrologLiteral b n f)
        | n == "" || nub n == " " = empty
        | otherwise =
           hcat
             [ begin
             , myText n
             , char '('
             , separated
             , char ')'
             , end
             ]
      where
        separated = myText $ intercalate "," f
        (begin,end) = if b then (empty,empty) else (text "not(", char ')')




instance Pretty PrologClause where
    pretty pc
        | Set.null (pLiterals pc) = text "{ }"
        | otherwise = hsep $ punctuate (text " ∨ ") $ map pretty $ terms pc



instance Pretty PickInst where
  pretty  PickInst{..} =
      text "PickInst(" <> vcat
                           [ nest 2 $ pretty formulas
                           , char ',' <+> pretty correct
                           , myText (", {" ++ show showSolution ++ "}")
                           , maybe empty (\s -> myText (", {" ++ show (toList s) ++ "}")) addText
                           , char ')'
                           ]


instance Pretty FormulaInst where
  pretty (InstCnf cnf) = text "Cnf{" <> pretty cnf <> char '}'
  pretty (InstDnf dnf) = text "Dnf{" <> pretty dnf <> char '}'
  pretty (InstArbitrary tree) = text "SynTree{" <> pretty tree <> char '}'



-- show tables side by side
showIndexedList :: Show b => Int -> Int -> [b] -> String
showIndexedList _ _ []= ""
showIndexedList maxLine gapSize xs = prodLines indexed
  where
    strings = map (lines . show) xs
    lineLen = length $ head $ head strings
    indices = [show num ++ replicate (lineLen - length (show num)) ' ' | num <- [1..length xs]]
    indexed = zipWith (:) indices strings
    gaps = replicate gapSize ' '
    sperseGaps = replicate (length $ head indexed) gaps
    perLine = maxLine `div` (lineLen + gapSize)

    prodLines ys
        | null ys = ""
        | perLine <= length ys = unlines [ unlines (row tRow)
                                         , " "
                                         , prodLines rest
                                         ]
        | otherwise = unlines (row ys)
      where
        tRow = take perLine ys
        rest = drop perLine ys
        row ls = foldl1 (zipWith (++)) (intersperse sperseGaps ls)

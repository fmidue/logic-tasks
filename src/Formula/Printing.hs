{-# OPTIONS_GHC -fno-warn-orphans #-}
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





myText :: String -> Doc
myText = text . pack




instance Pretty Number where
  pretty num = maybe empty pretty $ value num



instance Pretty TruthValue where
  pretty tv = pretty $ truth tv


instance Pretty ResStep where
  pretty (Res (a,b,(c,_))) = tupled [litsOrNum a, litsOrNum b, prettyClause c]
    where
      litsOrNum = either prettyClause pretty
      curlyBracesList = encloseSep (char '{') (char '}') (char ',')
      prettyClause = curlyBracesList . map pretty . literals



instance Pretty Literal where
   pretty (Literal x) = char x
   pretty (Not x) = myText ['~', x]



instance Pretty Clause where
    pretty clause = listShow $ literals clause
      where
        listShow [] = empty
        listShow [x] = pretty x
        listShow (x:xs) = hsep [pretty x, text "\\/", listShow xs]



instance Pretty Con where
    pretty con = listShow $ literals con
      where
        listShow [] = empty
        listShow [x] = pretty x
        listShow (x:xs) = hsep [pretty x, text "/\\", listShow xs]




instance Pretty Cnf where
    pretty cnf = listShow $ getClauses cnf
      where
        listShow [] = empty
        listShow [x] = singlePrint x
        listShow (x:xs) = hsep
                           [ singlePrint x
                           , text "/\\"
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
                           , text "\\/"
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
        | Set.null (pliterals pc) = text "{ }"
        | otherwise = hsep $ punctuate (text " \\/ ") $ map pretty $ terms pc



instance Pretty PickInst where
  pretty  PickInst{..} =
      text "PickInst(" <> vcat
                           [ nest 2 $ pretty cnfs
                           , char ',' <+> pretty correct
                           , maybe empty (\s -> myText (", {" ++ s ++ "}")) addText
                           , char ')'
                           ]





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


module Formula.Table
       (
         readEntries
       , readAtomics
       , flipAt
       , gapsAt
       ) where



import Data.Set (Set)

import Formula.Types
import qualified Data.Set as Set (fromList)




readEntries :: Table -> [Maybe Bool]
readEntries = getEntries

-- Used in Autotool
readAtomics :: Table -> Set Char
readAtomics = Set.fromList . getAtomics



gapsAt :: Table -> [Int] -> Table
gapsAt table gaps = Table (getAtomics table) newEntries
  where
    entries = getEntries table
    newEntries = [ if x+1 `elem` gaps then Nothing else entries !! x
                 | x <- [0..length entries-1]
                 ]





flipAt :: Table -> [Int] -> Table
flipAt table indices = Table (getAtomics table) newEntries
  where
    entries = getEntries table
    newEntries = [ let value = entries !! x in
                     if x+1 `elem` indices then fmap not value else value
                 | x <- [0..length entries-1]
                 ]


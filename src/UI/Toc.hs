module UI.Toc 
    ( filterVisible 
    ) where

import Types (TocEntry(..))
import Data.Set (Set)
import qualified Data.Set as Set

filterVisible :: Set Int -> [(Int, TocEntry, Bool)] -> [(Int, TocEntry, Bool)]
filterVisible _ [] = []
filterVisible collapsed ((i, e, hc):xs) =
    let current = (i, e, hc)
    in if hc && Set.member i collapsed
       then current : filterVisible collapsed (dropWhile (\(_, child, _) -> tocLevel child > tocLevel e) xs)
       else current : filterVisible collapsed xs

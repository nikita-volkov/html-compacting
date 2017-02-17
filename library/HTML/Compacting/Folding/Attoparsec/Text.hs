module HTML.Compacting.Folding.Attoparsec.Text
where

import HTML.Prelude hiding (filter, foldMap)
import qualified Control.Foldl as A
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as C
import qualified Data.Attoparsec.Text as D
import qualified HTML.Compacting.Folding.Attoparsec.Common as E


-- |
-- Turn a text parser into a fold.
{-# INLINE parser #-}
parser :: D.Parser a -> A.Fold Text (Either Text a)
parser parser =
  fmap fst (E.result (D.Partial (D.parse parser)))

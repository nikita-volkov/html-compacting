module HTML.Compacting.Folding.Attoparsec.Common
where

import HTML.Prelude hiding (filter, foldMap)
import qualified Control.Foldl as A
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as C
import Data.Attoparsec.Text


-- |
-- Turn an input-agnostic result into a reducer.
{-# INLINE result #-}
result :: (Monoid input) => IResult input a -> A.Fold input (Either Text a, input)
result result =
  A.Fold progress result exit
  where
    progress state chunk =
      case state of
        Partial chunkToResult ->
          chunkToResult chunk
        _ ->
          state
    exit =
      \case
        Partial chunkToResult ->
          exit (chunkToResult mempty)
        Done leftovers resultValue ->
          (Right resultValue, leftovers)
        Fail leftovers contexts message ->
          (Left (fromString resultMessage), leftovers)
          where
            resultMessage =
              if null contexts
                then message
                else showString (intercalate " > " contexts) (showString ": " message)

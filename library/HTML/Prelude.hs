module HTML.Prelude
( 
  module Exports,
  list,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (First(..), Last(..), (<>))

-- semigroups
-------------------------
import Data.Semigroup as Exports

-- text
-------------------------
import Data.Text as Exports (Text)


{-# INLINE list #-}
list :: a -> (b -> [b] -> a) -> [b] -> a
list nil cons =
  \case
    head : tail -> cons head tail
    _ -> nil

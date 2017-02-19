module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.XML.Types
import qualified HTML.Compacting as A
import qualified Control.Foldl as B


main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "" $
    assertEqual "" [element "p" [" a b", element "br" []]] (A.process [element "p" ["  a ", "", " b", element "br" []]])
  ]

instance IsString Node where
  fromString =
    NodeContent . ContentText . fromString

element :: Text -> [Node] -> Node
element name nodes =
  NodeElement (Element (Name name Nothing Nothing) [] nodes)

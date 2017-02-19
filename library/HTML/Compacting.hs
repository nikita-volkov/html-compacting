module HTML.Compacting
(
  process,
)
where

import HTML.Prelude
import Data.XML.Types
import qualified HTML.Compacting.Folding.Node as A
import qualified HTML.Compacting.Folding.Text as B
import qualified HTML.Compacting.Folding.Transducer as C
import qualified Control.Foldl as D
import qualified Data.HashMap.Strict as E


process :: [Node] -> [Node]
process =
  foldMap $ \case
    NodeElement (Element name attributes nodes) ->
      return (NodeElement (Element name attributes newNodes))
      where
        newNodes =
          process (D.fold (transducer D.list) nodes)
          where
            transducer =
              case name of
                Name localName Nothing Nothing -> lookupTransducer localName
                _ -> defaultTransducer
              where
                lookupTransducer localName =
                  E.lookupDefault defaultTransducer localName hashMap
                  where
                    hashMap =
                      E.fromList list
                      where
                        list =
                          mconcat
                          [
                            group A.removingSpaceBetweenTags $
                            [
                              "html",
                              "head",
                              "body",
                              "base",
                              "link",
                              "meta",
                              "title",
                              "table",
                              "tbody",
                              "tr",
                              "colgroup",
                              "col",
                              "optgroup",
                              "select",
                              "ul",
                              "ol",
                              "dl",
                              "form"
                            ]
                            ,
                            group id $
                            [
                              "pre"
                            ]
                          ]
                          where
                            group transducer =
                              map (\name -> (name, transducer))
                              
                defaultTransducer =
                  A.compressingSpace
    _ ->
      mempty

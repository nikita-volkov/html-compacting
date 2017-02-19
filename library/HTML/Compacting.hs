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
  transduce A.compressingSpace

transduce :: C.Transducer Node Node -> [Node] -> [Node]
transduce transducer =
  D.fold ((C.map (mapElement processElement) . transducer) D.list)

mapElement :: (Element -> Element) -> Node -> Node
mapElement mapping =
  \case
    NodeElement element -> NodeElement (mapping element)
    node -> node

processElement :: Element -> Element
processElement (Element name attributes nodes) =
  Element name attributes newNodes
  where
    newNodes =
      transduce transducer nodes
      where
        transducer :: C.Transducer Node Node
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

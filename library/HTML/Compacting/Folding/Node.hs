module HTML.Compacting.Folding.Node
where

import HTML.Prelude hiding (filter)
import Data.XML.Types
import qualified Control.Foldl as A
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as C
import qualified HTML.Compacting.Folding.Text as D
import qualified HTML.Compacting.Folding.Transducer as E


transducingContentText :: E.Transducer Text Text -> E.Transducer Node Node
transducingContentText textTransducer =
  reducingContentText (textTransducer D.concat)

reducingContentText :: A.Fold Text Text -> E.Transducer Node Node
reducingContentText (A.Fold textStep textInit textExit) (A.Fold nodeStep nodeInit nodeExit) =
  A.Fold newStep newInit newExit
  where
    flushTextState state =
      \case
        Just textState -> case textExit textState of
          "" -> state
          text -> nodeStep state (NodeContent (ContentText text))
        Nothing -> state
    newStep (textState, nodeState) node =
      case node of
        NodeContent (ContentText text) -> (Just (textStep (fromMaybe textInit textState) text), nodeState)
        _ -> (Nothing, nodeStep (flushTextState nodeState textState) node)
    newInit =
      (Nothing, nodeInit)
    newExit (textState, nodeState) =
      nodeExit (flushTextState nodeState textState)

trimming :: E.Transducer Node Node
trimming =
  transducingContentText D.trimming

{-|
Compress the spaces in the text nodes.
-}
compressingSpace :: E.Transducer Node Node
compressingSpace =
  transducingContentText D.compressingSpace

removingSingleSpaceContentTexts :: E.Transducer Node Node
removingSingleSpaceContentTexts =
  E.filter $ \case
    NodeContent (ContentText " ") -> False
    _ -> True

removingSpaceBetweenTags :: E.Transducer Node Node
removingSpaceBetweenTags =
  compressingSpace . removingSingleSpaceContentTexts

recursing :: E.Transducer Node Node -> E.Transducer Node Node
recursing transducer (A.Fold step init exit) =
  A.Fold newStep init exit
  where
    newStep state node =
      step state traversedNode
      where
        traversedNode =
          case node of
            NodeElement (Element name attributes nodes) ->
              NodeElement (Element name attributes (A.fold (recursing transducer A.list) nodes))
            _ ->
              node
                


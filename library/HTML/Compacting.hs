module HTML.Compacting
where

import HTML.Prelude
import Data.XML.Types
import qualified HTML.Compacting.Folding.Node as A
import qualified HTML.Compacting.Folding.Text as B
import qualified HTML.Compacting.Folding.Transducer as C
import qualified Control.Foldl as D


{-|
A rule for a specific element.
-}
newtype Rule =
  Rule (Element -> Maybe Element)

asIs :: Rule
asIs =
  Rule Just

havingLocalName :: Text -> Rule -> Rule
havingLocalName name (Rule ruleDef) =
  Rule newRuleDef
  where
    newRuleDef element@(Element (Name actualLocalName _ _) _ _) =
      if actualLocalName == name
        then ruleDef element
        else Nothing

transducingNodes :: C.Transducer Node Node -> Rule -> Rule
transducingNodes transducer (Rule ruleDef) =
  Rule (fmap (updateNodes (D.fold (transducer D.list))) . ruleDef)
  where
    updateNodes update element =
      element { elementNodes = update (elementNodes element) }

transducingText :: C.Transducer Text Text -> Rule -> Rule
transducingText transducer =
  transducingNodes (A.transducingContentText transducer)

fullyNamedNodeTransducer :: Name -> C.Transducer Node Node -> Rule
fullyNamedNodeTransducer name transducer =
  Rule def
  where
    def (Element actualName attributes nodes) =
      if actualName == name
        then Just newElement
        else Nothing
      where
        newElement =
          Element name attributes newNodes
          where
            newNodes =
              D.fold (transducer D.list) nodes

namedNodeTransducer :: Text -> C.Transducer Node Node -> Rule
namedNodeTransducer name =
  fullyNamedNodeTransducer (Name name Nothing Nothing)

html :: Rule
html =
  transducingNodes transducer (havingLocalName "html" asIs)
  where
    transducer =
      A.removingSpaceBetweenTags

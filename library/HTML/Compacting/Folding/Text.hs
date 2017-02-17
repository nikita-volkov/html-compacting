module HTML.Compacting.Folding.Text
where

import HTML.Prelude hiding (filter, foldMap)
import Control.Foldl
import qualified Data.Text as A
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as C
import qualified HTML.Compacting.Attoparsec.Text as D
import qualified HTML.Compacting.Folding.Attoparsec.Text as E
import qualified HTML.Compacting.Folding.Transducer as F


concat :: Fold Text Text
concat =
  foldMap B.fromText (C.toStrict . B.toLazyText)

compressSpace :: Fold Text Text
compressSpace =
  either (error . A.unpack . (<>) "Bug: Unexpected parser failure: ") id <$>
  E.parser D.textWithCompressedSpaces

compressingSpace :: F.Transducer Text Text
compressingSpace (Fold progress enter exit) =
  Fold newProgress newEnter newExit
  where
    newProgress (aggregatingSpace, deeperState) chunk =
      case A.span isSpace chunk of
        (headSpace, afterSpace) ->
          case A.break isSpace afterSpace of
            (notSpace, afterNotSpace) ->
              if A.null notSpace
                then
                  newProgressIfNotEmpty (aggregatingSpace || not (A.null headSpace), deeperState) afterNotSpace
                else
                  if aggregatingSpace || not (A.null headSpace)
                    then newProgressIfNotEmpty (False, progress (progress deeperState " ") notSpace) afterNotSpace
                    else newProgressIfNotEmpty (False, progress deeperState notSpace) afterNotSpace
              where
                newProgressIfNotEmpty state chunk =
                  if A.null chunk
                    then state
                    else newProgress state chunk
    newEnter =
      (False, enter)
    newExit (aggregatingSpace, deeperState) =
      exit (if aggregatingSpace then progress deeperState " " else deeperState)

trimmingFromLeft :: F.Transducer Text Text
trimmingFromLeft (Fold progress enter exit) =
  Fold newProgress newEnter newExit
  where
    newProgress (trimmed, deeperState) chunk =
      if trimmed
        then (trimmed, progress deeperState chunk)
        else case A.span isSpace chunk of
          (headSpace, afterSpace) ->
            if A.null afterSpace
              then (trimmed, deeperState)
              else (True, progress deeperState afterSpace)
    newEnter =
      (False, enter)
    newExit (aggregatingSpace, deeperState) =
      exit deeperState

trimmingFromRight :: F.Transducer Text Text
trimmingFromRight (Fold progress enter exit) =
  Fold newProgress newEnter newExit
  where
    newProgress (spaceChunks, deeperState) chunk =
      case A.break isSpace chunk of
        (a, b) ->
          if A.null a
            then case A.span isSpace b of
              (c, d) ->
                newProgress (newSpaceChunks, deeperState) d
                where
                  newSpaceChunks =
                    if A.null c
                      then spaceChunks
                      else c : spaceChunks
            else newProgress ([], progress (foldr (flip progress) deeperState spaceChunks) a) b
    newEnter =
      ([], enter)
    newExit (_, deeperState) =
      exit deeperState

trimming :: F.Transducer Text Text
trimming =
  trimmingFromRight . trimmingFromLeft

module HTML.Compacting.Attoparsec.Text
where

import HTML.Prelude
import Data.Attoparsec.Text
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as C


textWithCompressedSpaces :: Parser Text
textWithCompressedSpaces =
  C.toStrict . B.toLazyText <$> textBuilderWithCompressedSpaces

textBuilderWithCompressedSpaces :: Parser B.Builder
textBuilderWithCompressedSpaces =
  build mempty
  where
    build state =
      (chunk >>= build) <|> pure state
      where
        chunk =
          compressedSpace <|> notSpace
          where
            compressedSpace =
              B.singleton ' ' <$ skipWhile1 isSpace
            notSpace =
              B.fromText <$> takeWhile1 (not . isSpace)

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 predicate =
  satisfy predicate *> skipWhile predicate

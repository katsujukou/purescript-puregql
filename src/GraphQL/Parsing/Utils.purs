module GraphQL.Parsing.Utils where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array (elem)
import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Array.ST as STArray
import Data.Bifunctor (lmap)
import Data.Enum (toEnum)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Set (member)
import Data.Set as Set
import Data.String (CodePoint, codePointFromChar, fromCodePointArray, length, toCodePointArray)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.Tuple.Nested (type (/\), (/\))

punctuatorChars :: String
punctuatorChars = "!$().:=@[]{|}"

isPunctuator :: CodePoint -> Boolean
isPunctuator cp = cp `member` Set.fromFoldable (toCodePointArray punctuatorChars)

isAlphabet :: CodePoint -> Boolean
isAlphabet = (_ `member` Set.fromFoldable (toCodePointArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

isDigit :: CodePoint -> Boolean
isDigit = (_ `member` Set.fromFoldable (toCodePointArray "0123456789"))

isBeginningOfName :: CodePoint -> Boolean
isBeginningOfName = isAlphabet \/ (_ `is` '_')

isName :: CodePoint -> Boolean
isName = isBeginningOfName \/ isDigit

isBeginningOfNumericValue :: CodePoint -> Boolean
isBeginningOfNumericValue = isDigit \/ (_ `is` '-')

cr :: CodePoint
cr = codePointFromChar '\r'

lf :: CodePoint
lf = codePointFromChar '\n'

isWhitespace :: CodePoint -> Boolean
isWhitespace = (_ `elem` toCodePointArray " \t")

isStringCharacter :: CodePoint -> Boolean
isStringCharacter = isSourceCharacter ./\ (_ `not <<< elem` toCodePointArray "\\\"\n")

isSourceCharacter :: CodePoint -> Boolean
isSourceCharacter cp
  | cp `elem` toCodePointArray "\t\r\n" = true
  | Just true <- (>=) <$> (Just cp) <*> (toEnum 0x0020)
  , Just true <- (<=) <$> (Just cp) <*> (toEnum 0xFFFF) = true
  | otherwise = false

is :: CodePoint -> Char -> Boolean
is cp ch = cp == codePointFromChar ch

isn't :: CodePoint -> Char -> Boolean
isn't = not <<< is

--| logical or
lor :: forall a. (a -> Boolean) -> (a -> Boolean) -> a -> Boolean
lor p q = \b -> p b || q b

infixr 4 lor as \/

--| logical and
land :: forall a. (a -> Boolean) -> (a -> Boolean) -> a -> Boolean
land p q = \b -> p b && q b

infixr 3 land as ./\

takeWhile :: (CodePoint -> Boolean) -> String -> String /\ String
takeWhile p = lmap (fromCodePointArray <<< Array.fromFoldable <<< List.reverse) <<< go List.Nil
  where
  go acc = String.uncons >>> case _ of
    Nothing -> acc /\ ""
    Just { head: cp, tail: rest }
      | p cp -> go (cp `List.Cons` acc) rest
      | otherwise -> acc /\ (String.singleton cp <> rest)

spanRegex :: Regex -> String -> String /\ String
spanRegex regex inp = Regex.match regex inp >>= NE.head
  # maybe ("" /\ inp) \matched -> matched /\ (String.drop (length matched) inp)

-- Taking a predicate `t -> Boolean` and an array of `t`, 
-- groups an input array by applying predicate to each element of array,
-- delimiting the element which does not satisfy the predicate.
-- For example:
-- ```purs
-- splicChunksWith even [1,3,5,2,7,11,4,15] == [[1,3,5], [7,11], [15]] 
-- ```
splitChunksWith :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
splitChunksWith p xs = ST.run do
  inpRef <- STRef.new xs
  resultsRef <- STArray.new
  STRef.while ((not <<< Array.null) <$> STRef.read inpRef) do
    chunk <- Array.takeWhile (not <<< p) <$> STRef.read inpRef
    _ <- STRef.modify (Array.drop (1 + Array.length chunk)) inpRef
    STArray.push chunk resultsRef
  STArray.unsafeFreeze resultsRef
module Bob (responseFor) where

import Data.Char (isLetter)
import Data.Text (Text)
import qualified Data.Text as T

isQuestion :: Text -> Bool
isQuestion = T.isSuffixOf (T.pack "?")

isYelling :: Text -> Bool
isYelling text = T.toUpper text == text && T.any isLetter text

responseFor :: Text -> Text
responseFor xs
  | isQuestion (T.strip xs) && isYelling xs = T.pack "Calm down, I know what I'm doing!"
  | isQuestion (T.strip xs) = T.pack "Sure."
  | isYelling xs = T.pack "Whoa, chill out!"
  | T.strip xs == T.pack "" = T.pack "Fine. Be that way!"
  | otherwise = T.pack "Whatever."

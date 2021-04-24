module DNA (toRNA) where

transcribe :: Char -> Either Char String
transcribe x = case x of
  'G' -> Right "C"
  'C' -> Right "G"
  'T' -> Right "A"
  'A' -> Right "U"
  _ -> Left x

toRNA :: String -> Either Char String
toRNA xs = concat <$> mapM transcribe xs

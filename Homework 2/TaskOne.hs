module TaskOne where

safeInit :: [a] -> Either String [a]
safeInit [] = Left "List is too small"
safeInit l  = Right $ init l

safeTail :: [a] -> Either String [a]
safeTail [] = Left "List is too small"
safeTail l  = Right $ tail l

strip :: [a] -> [a]
strip l = case safeTail l of
          Left msg -> error msg
          Right l  -> case safeInit l of
                      Left msg -> error msg
                      Right l  -> l

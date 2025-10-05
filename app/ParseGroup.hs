module ParseGroup where

getGroup :: String -> String
getGroup pattern = takeWhile (/=']') pattern

-- Possible could be implimented with (Either a b) type and folds or some map
find :: (Eq a) => a -> [a] -> Either Int Int
find _ [] = Left (-1)
find elem (x:list)
    | elem == x = Right 0
    | otherwise = fmap (+1) $ find elem list

unfold :: String -> [String]
unfold [] = []
unfold all@(_:remain) = [all] ++ unfold remain

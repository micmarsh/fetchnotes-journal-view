module Utils where

resolve :: [Maybe a] -> [a]
resolve maybes = resolve' [] maybes
    where
        resolve' acc [] = reverse acc
        resolve' acc ((Just thing) : rest) = resolve' (thing : acc) rest
        resolve' acc (Nothing : rest) = resolve' acc rest

safeHead :: [a] -> Maybe a
safeHead list =
    if length list > 0
    then Just (head list)
    else Nothing
module Functions where

    isSingleton :: [a] -> Bool
    isSingleton [_] = True
    isSingleton _ = False

    rev list = if null list then [ ]
        else rev (tail list) ++ [head list]

    inRange low high list = [x | x<-list, x>=low, x<= high]

    zipWith f xs ys = map (uncurry f) (zip xs ys)

    qsort [ ] = [ ]
    qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger where
                    smaller = [y | y<-xs, y<x]
                    larger = [y | y<-xs, y>= x]

    member :: (Eq a) => a->[a]->Bool
    member _ [] = False
    member x (h:t) = (x==h) || (member x t)

    numToBool :: Integer->Bool
    numToBool 0 = False
    numToBool _ = True

    boolToNum :: Bool->Integer
    boolToNum False = 0
    boolToNum True = 1

    listify :: [a] -> [[a]]
    listify [] = []
    listify (x:xs) = [x]:(listify xs)

    removeLast :: [a]->[a]
    removeLast (x:[]) = []
    removeLast (x:xs) = x:(removeLast xs)

    findFirst :: (a->Bool) -> [a] -> a
    findFirst _ [] = error "no element meets condition\n"
    findFirst f (x:xs)
        | (f x) = x
        | otherwise = findFirst f xs

    argmax :: (a -> Integer)->[a]->a
    argmax _ [] = error "it's empty bruh"
    argmax f (x:xs) = findFirst (\c -> ((f c) == (maximum (map f (x:xs))))) (x:xs)

    argmin :: (a -> Integer)->[a]->a
    argmin _ [] = error "it's empty bruh"
    argmin f (x:xs) = findFirst (\c -> ((f c) == (minimum (map f (x:xs))))) (x:xs)

    argmaxLast :: (a -> Integer)->[a]->a
    argmaxLast _ [] = error "it's empty bruh"
    argmaxLast f l = argmax f (rev l)

    argminLast :: (a -> Integer)->[a]->a
    argminLast _ [] = error "it's empty bruh"
    argminLast f l = argmin f (rev l)

    splitBy :: (Eq a)=>a->[a]->([a], [a])
    splitBy _ [] = ([],[])
    splitBy p (x:xs)
        | p == x = ([], xs)
        | otherwise = (x:(fst newList), snd newList)
        where newList = splitBy p xs



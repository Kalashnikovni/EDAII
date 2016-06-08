import Par
import Seq

--TABULATE
tabulateL :: (Int -> a) -> Int -> [a]
tabulateL f n 
    | n == 0 = [f 0]
    | n > 0  = let (a,b) = f n ||| (tabulateL f (n-1))
               in appendS b [a]


--MAP
mapL :: (a -> b) -> [a] -> [b]
mapL _ []     = []
mapL f (x:xs) = let (a,b) = f x ||| (map f xs)
                in a:b 


--FILTER
filterL :: (a -> Bool) -> [a] -> [a]
filterL _ [] = []
filterL f (x:xs) = let (a,b) = f x ||| (filter f xs)
                   in if a then x:b else b


--SHOWT
showtL :: [a] -> TreeView a [a]
showtL []  = EMPTY
showtL [a] = ELT a
showtL xs  = let 
                n      = quot (lengthS xs) 2
                (l, r) = (takeS xs n) ||| (dropS xs n)
             in NODE l r


--CONCAT
concatL :: [[a]] -> [a]
concatL []          = []
concatL [xs]        = xs
concatL (xs:ys:zss) = let (a,b) = (appendS xs ys) ||| (concatL zss)
                      in appendS a b


--REDUCE
reduceL :: (a -> a -> a) -> a -> [a] -> a
reduceL _ n [] = n
reduceL f n xs = f n (reduceL' f xs)

contraer :: (a -> a -> a) -> [a] -> [a]
contraer _ []       = []
contraer _ [x]      = [x]
contraer f (x:y:xs) = let (a,b) = (f x y) ||| (contraer f xs)
                      in a:b

reduceL' :: (a -> a -> a) -> [a] -> a
reduceL' _ [x]      = x 
reduceL' f [x,y]    = f x y
reduceL' f xs       = reduceL' f (contraer f xs)


--SCAN
expandir :: (a -> a -> a) -> [a] -> [a] -> [a]
expandir _ [] []           = []
expandir _ [_] zs          = zs
expandir f (x:_:xs) (z:zs) = let (a,b) = (f z x) ||| (expandir f xs zs)
                             in z:a:b

scanL :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanL f n xs = (scanL' f n xs) ||| (reduceS f n xs)

scanL' :: (a -> a -> a) -> a -> [a] -> [a]
scanL' _ n [_] = [n]
scanL' f n xs  = expandir f xs (scanL' f n (contraer f xs))

--SHOWL
showlL :: [a] -> ListView a [a]
showlL []     = NIL
showlL (x:xs) = CONS x xs


instance Seq [] where
    emptyS       = []
    singletonS x = [x]
    lengthS      = length
    nthS         = (!!) 
    tabulateS    = tabulateL
    mapS         = mapL
    filterS      = filterL
    appendS      = (++)
    takeS xs n   = take n xs
    dropS xs n   = drop n xs
    showtS       = showtL
    showlS       = showlL
    joinS        = concatL
    reduceS      = reduceL
    scanS        = scanL
    fromList     = id

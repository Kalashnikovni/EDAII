-- Scan esta mal implementado, probar +3
-- Usando tabulateS?

import Par
import Seq
import qualified Arr as A
import Arr ((!))

--SHOWT
showtA :: A.Arr a -> TreeView a (A.Arr a)
showtA xs | lengthS xs == 0 = EMPTY
	  | lengthS xs == 1 = ELT (nthS xs 0)
	  | otherwise       = NODE (takeS xs (quot (lengthS xs) 2)) (dropS xs (quot (lengthS xs) 2))

--SHOWL
showlA :: A.Arr a -> ListView a (A.Arr a)
showlA xs | lengthS xs == 0 = NIL
	  | otherwise       = CONS (nthS xs 0) (dropS xs 1)

--TO_TREE
toTree :: A.Arr a -> Tree a
toTree xs | len == 0 = Empty
          | len == 1 = Leaf (nthS xs 0)
          | otherwise       = let (a,b) = (toTree (takeS xs (part len))) ||| (toTree (dropS xs (part len)))
                              in Node a b
	  where len = lengthS xs
                part x = floor (2 ^ ((floor . logBase 2.0 . fromIntegral) (x - 1)))

--FILTER
filterA :: (a -> Bool) -> A.Arr a -> A.Arr a
filterA f xs = joinS (tabulateS (\i -> if f (nthS xs i) then singletonS (nthS xs i) else emptyS) (lengthS xs)) 

--REDUCE
reduceA :: (a -> a -> a) -> a -> A.Arr a -> a
reduceA f n xs = f n (reduceA' f n (toTree xs))

reduceA' :: (a -> a -> a) -> a -> Tree a -> a
reduceA' f n Empty        = n
reduceA' f n (Leaf x)     = x
reduceA' f n (Node lt rt) = let (a,b) = (reduceA' f n lt) ||| (reduceA' f n rt) 
                            in f a b

--SCAN
contraer :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a
contraer f n xs
    | len == 0  = emptyS
    | len == 1  = xs
    | len == 2  = singletonS (f (nthS xs 0) (nthS xs 1))
    | otherwise = let (a,b) = (contraer f n (takeS xs hm)) ||| (contraer f n (dropS xs hm))
                  in appendS a b
      where hm = floor (2 ^ ((floor . logBase 2.0 . fromIntegral) (len - 1)))
            len = lengthS xs

scanA :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanA f n xs = (tabulateS (\i -> reduceS f n (takeS xs i)) ((lengthS xs))) ||| (reduceS f n xs)

expandir' :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a) -> (A.Arr a, a)
expandir' f n xs (ys, z) = (expandir f n xs ys, z)

expandir :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a -> A.Arr a
expandir f n xs zs 
        | lengthS xs == 0 && ((lengthS zs) == 0)    = emptyS
--        | lengthS zs == 0                           = singletonS (reduceS f n (takeS xs ((lengthS xs) - 1)))
--        | lengthS xs == 0                           = xs
	| lengthS xs == 1                           = takeS zs 1 
	| otherwise                                 = 
                              let c = takeS zs 1
                                  (a,b) = (f (nthS zs 0) (nthS xs 0)) ||| (expandir f n (dropS xs 2) (dropS zs 1))
			      in appendS (appendS c (singletonS a)) b

instance Seq A.Arr where
    emptyS = A.empty
    singletonS x = A.fromList [x]
    lengthS = A.length
    nthS = (!)
    tabulateS = A.tabulate
    mapS = \f -> (\xs -> tabulateS (\i -> (f (nthS xs i))) (lengthS xs))
    filterS = filterA 
    appendS = \s -> (\t -> tabulateS (\i -> if i < (lengthS s) then nthS s i else nthS t (i - (lengthS s))) ((lengthS s) + (lengthS t)))
    takeS xs n = A.subArray 0 n xs
    dropS xs n = A.subArray n (lengthS xs - n) xs
    showtS = showtA
    showlS = showlA
    joinS = A.flatten
    reduceS = reduceA
    scanS f n xs =  expandir' f n xs arg where arg = scanA f n (contraer f n xs)
    fromList = A.fromList

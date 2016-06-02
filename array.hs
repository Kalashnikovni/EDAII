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
contraer :: (a -> a -> a) -> a -> Tree a -> Tree a
contraer f n Empty                    = Empty
contraer f n (Leaf x)                 = Leaf x
contraer f n (Node (Leaf x) (Leaf y)) = Leaf (f x y)
contraer f n (Node lt rt)             = let (a,b) = (contraer f n lt) ||| (contraer f n rt)
                                        in Node a b  

scanA :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanA f n xs = (expandir f xs (appendS (singletonS n) (scanA' f n (contraer f n (toTree xs)))), reduceS f n xs)

scanA' :: (a -> a -> a) -> a -> Tree a -> A.Arr a
scanA' f n Empty              = singletonS n
scanA' f n (Leaf x)           = singletonS x
scanA' f n (Node (Leaf x) rt) = appendS (singletonS x) (scanA' f n rt)
scanA' f n (Node lt (Leaf y)) = appendS (scanA' f n lt) (singletonS y)
scanA' f n (Node lt rt)       = let (a,b) = (scanA' f n lt) ||| (scanA' f n rt)
                                in appendS a b

expandir :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expandir f xs zs 
        | lengthS xs == 0 && ((lengthS zs) == 0)    = emptyS
	| (lengthS xs) == 1 = zs 
	| otherwise         = let c = takeS zs 1
                                  (a,b) = (f (nthS zs 0) (nthS xs 0)) ||| (expandir f (dropS xs 2) (dropS zs 1))
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
    dropS xs n = A.subArray n ((lengthS xs) - n) xs
    showtS = showtA
    showlS = showlA
    joinS = A.flatten
    reduceS = reduceA
--    scanS = expandir (scanA)
    fromList = A.fromList

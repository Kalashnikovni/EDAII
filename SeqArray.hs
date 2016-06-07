import Par
import Seq
import qualified Arr as A
import Arr ((!))


--SHOWT
showtA :: A.Arr a -> TreeView a (A.Arr a)
showtA xs 
    | lengthS xs == 0 = EMPTY
	| lengthS xs == 1 = ELT (nthS xs 0)
	| otherwise       = NODE (takeS xs len) (dropS xs len)
      where len = quot (lengthS xs) 2


--SHOWL
showlA :: A.Arr a -> ListView a (A.Arr a)
showlA xs 
    | lengthS xs == 0 = NIL
	| otherwise       = CONS (nthS xs 0) (dropS xs 1)


--FILTER
filterA :: (a -> Bool) -> A.Arr a -> A.Arr a
filterA f xs = joinS (tabulateS (\i -> if f (nthS xs i) then singletonS (nthS xs i) else emptyS) (lengthS xs)) 


--REDUCE
reduceA :: (a -> a -> a) -> a -> A.Arr a -> a
reduceA f n xs 
    | lengthS xs == 0 = n
    | otherwise       = f n (reduceA' f xs)

reduceA' :: (a -> a -> a) -> A.Arr a -> a
reduceA' f xs
    | lengthS xs == 1  = nthS xs 0
    | otherwise        = reduceA' f (contraer f xs)

--SCAN
contraer :: (a -> a -> a) -> A.Arr a -> A.Arr a
contraer f xs
    | even len  = tabi 
    | odd len   = appendS tabi (singletonS (nthS xs (len-1))) 
      where len  = lengthS xs
            tabi = tabulateS (\i -> f (nthS xs (2*i)) (nthS xs (2*i + 1))) (quot len 2) 

expandir :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expandir f xs zs = tabulateS (\i -> if even i then (nthS zs (quot i 2)) else f (nthS zs (quot i 2)) (nthS xs (i-1))) (lengthS xs)

scanA :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanA f n xs = (scanA' f n xs) ||| (reduceS f n xs)

scanA' :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a
scanA' f n xs 
    | lengthS xs == 1 = singletonS n
    | otherwise       = expandir f xs (scanA' f n (contraer f xs))


instance Seq A.Arr where
    emptyS = A.empty
    singletonS = \x -> (A.fromList [x])
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
    scanS = scanA
    fromList = A.fromList

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
    | even len  = joinS (tabulateS (\i -> singletonS (f (nthS xs (2*i)) (nthS xs (2*i + 1)))) (quot len 2)) 
    | odd len   = appendS (joinS (tabulateS (\i -> singletonS (f (nthS xs (2*i)) (nthS xs (2*i + 1)))) (quot len 2))) (singletonS (nthS xs (len-1))) 
      where len = lengthS xs

expandir :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expandir f xs zs 
    | lengthS xs == 0 && ((lengthS zs) == 0)    = emptyS
    | lengthS zs == 0                           = xs
	| lengthS xs == 1                           = takeS zs 1 
	| otherwise                                 = 
         let c     = takeS zs 1
             (a,b) = (f (nthS zs 0) (nthS xs 0)) ||| (expandir f (dropS xs 2) (dropS zs 1))
		 in appendS (appendS c (singletonS a)) b

scanA :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanA f n xs = (scanA' f n xs) ||| (reduceS f n xs)
    -- Codigo viejo: scanA f n xs  = (tabulateS (\i -> reduceS f n (takeS xs i)) engthS xs)) ||| (reduceS f n xs)

scanA' :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a
scanA' f n xs 
    | lengthS xs == 1 = singletonS n
    | otherwise       = expandir f xs (scanA' f n (contraer f xs))
    --Cambio al mismo algoritmo de la instancia en listas


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

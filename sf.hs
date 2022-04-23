
import Data.List
import Data.Maybe

alphaNum = [ [c] | c <- "0123456789abcdefghijklmnopqrstuvwxyz" ]
alphabet :: Int -> Int -> [String]
alphabet 1 m = take m alphaNum
alphabet n m = concatMap (\y -> map (y ++) baseAlphabet) priorAlphabet
  where
    priorAlphabet = alphabet (n - 1) m
    baseAlphabet = alphabet 1 m



-- Finite functions
data Ff = Ff {
  name::String,
  alpha::[String],
  domain::Int,
  codomain::Int,
  func::[[Int]]
  }

instance Eq Ff where
  (==) f g = (==) (name f) (name g)

instance Ord Ff where
  compare f g = compare (name f) (name g)

instance Show Ff where
  show (Ff name alpha domain codomain func) =
    "\n" ++ name ++ "[" ++
    concatMap (\(i,l) -> if length l == 1 then "\n  [" ++ (alpha !! i) ++ "] = " ++ (alpha !! (head l))
          else
            "\n  " ++ alpha !! i ++ " could be { " ++ alpha !! head l ++ (concatMap (\e -> ", " ++ alpha !! e) (tail l)) ++ " }") (zip [0..length func - 1] func) ++
    "\n]\n"


anyp :: Int -> Int -> Ff
anyp d c = Ff ("any" ++ show d ++ "->" ++ show c) (alphabet 3 16) d c (take d $ repeat [0..c-1])

pick :: Ff -> Int -> [Ff]
pick f n = if length sl == 1 then [f] else
             map (\e -> sub e f n) sl
  where
    sl = ((func f) !! n)
    sub d g i = Ff ("pick" ++ (alpha f) !! i ++ "to" ++ (alpha f) !! d ++ "." ++ name g) (alpha g) (domain g) (codomain g)
      (map (\(j,r) -> if j == n then [d] else (filter (/= d) r)) (zip [0..domain f - 1] (func f)))

pickSubdomain :: Ff -> Int -> [Ff]
pickSubdomain f ss = last $ scanl (\fs n -> concatMap (\f -> pick f n) fs) [f] [0..ss-1]
  
compose :: Ff -> Ff -> Ff
compose f g = Ff ("(" ++ name f ++ ").(" ++ name g ++ ")") (alpha f) (codomain g) (codomain f) fi
  where
    fi = map (\x -> (map (\(_,y) -> y) x)) (groupBy bsrt $ sortBy srt $ (map head $ group (sort ppps)))
    ppps = concatMap (\(x,y) -> [ (x,z) | z <- (func f) !! y ]) pps
    pps = map head $ group (sort ps)
    srt a b = compare (fst a) (fst b)
    bsrt a b = (fst a) == (fst b)
    ps = concatMap (\(l,i) -> [ (i,x) | x <- l ]) (zip (func g) [0..domain g-1])

invert :: Ff -> Ff
invert f = Ff ("invert." ++ name f) (alpha f) (codomain f) (codomain f) fi
  where
    fi = map (\x -> (map (\(_,y) -> y) x)) (groupBy bsrt (sortBy srt ps))
    srt a b = compare (fst a) (fst b)
    bsrt a b = (fst a) == (fst b)
    ps = concatMap (\(l,i) -> [ (x,i) | x <- l ]) (zip (func f) [0..domain f-1])

main =
  do
    print $ tl
    print $ tli
    print $ compose tl tli
    print $ compose tli tl
  where
    tli = invert tl
    tl = last $ take (16*15*14*13*12) $ pickSubdomain (anyp 16 16) 4
    


import Data.List
import Data.Maybe

alphaNum = [ [c] | c <- "0123456789abcdefghijklmnopqrstuvwxyz" ]
alphabet :: Int -> Int -> [String]
alphabet 1 m = take m alphaNum
alphabet n m = concatMap (\y -> map (\x -> y ++ x) baseAlphabet) priorAlphabet
  where
    priorAlphabet = alphabet (n - 1) m
    baseAlphabet = alphabet 1 m



-- Finite functions
data Ff = Ff {
  name::String,
  alpha::[String],
  domain::Int,
  codomain::Int,
  func::[Int]
  } deriving (Eq, Ord)

instance Show Ff where
  show (Ff name alphabet domain codomain f) = "\n" ++ sub 0 
    where
      sub n = if n == domain then ""
        else name ++ "( " ++ alphabet !! n ++ " ) = " ++ alphabet !! (f !! n) ++ "\n" ++ sub (n + 1)


-- Combinations
comb :: Int -> [a] -> [[a]]
comb 1 l = [[c] | c <- l]
comb n l = concatMap (\f -> (map (\e -> f:e) p)) l
    where p = comb (n - 1) l


-- Finite function from list
ffl :: [Int] -> String -> [String] -> Ff
ffl ls n a = Ff n a (length ls) (maximum ls + 1) ls

nand = ffl [1, 1, 1, 0] "nand" (alphabet 2 2)
nor = ffl [1, 0, 0, 0] "nor" (alphabet 2 2)
cnot = ffl [0, 1, 3, 2] "cnot" (alphabet 2 2)
rot4 = ffl [1, 2, 3, 0] "rot4" (alphabet 2 2)
tofolli = ffl [0, 1, 2, 3, 4, 5, 7, 6] "tofolli" (alphabet 3 2)
fredkin = ffl [0, 1, 2, 3, 4, 6, 5, 7] "fredkin" (alphabet 3 2)
-- f(g(x)) h(f(x))


-- All functions with a given domain and codomain
affs :: Int -> Int -> [Ff]
affs d cd = map (\l -> ffl l ("allFuncs" ++ show d ++ "->" ++ show cd) (alphabet 1 (max d cd))) (comb d [0..cd-1])

-- All permutations with a given domain
afps :: Int -> [Ff]
afps n = map (\l -> ffl l ("allPerms" ++ show n) (alphabet 1 n)) (permutations [0..n-1])

-- Composition
cmp :: Ff -> Ff -> Ff
cmp f g = ffl (map (\e -> (func f) !! e) (func g)) (name g ++ "_then_" ++ name f) (alpha f) 

cmps :: [Ff] -> [Ff] -> [Ff]
cmps fs gs = [ cmp f g | f <- fs, g <- gs ]

--All inverses with respect to a subdomain
asdi :: Ff -> Int -> [Ff]
asdi f size = filter (\e -> [0..size-1] == take size (func (cmp f e)) && [0..size-1] == take size (func (cmp e f))  ) all
  where
    all = afps (codomain f) 

ex :: Ff -> Int -> Ff
ex f n = if n > domain f then
  Ff (name f) (alphabet 1 n) n n ([ (func f) !! e | e <- [0..(domain f)-1] ] ++ [(domain f)..n-1])
  else
  ffl (take n $ func f) (name f) (alpha f) 

morph :: Ff -> Ff -> [Ff]
morph f g = cmps [f] (cmps [(ex g (domain f))] finvs)
  where
    finvs = asdi f (domain g)

-- cmp :: Ffs -> Ffs -> Ffs
-- cmp f g = 

amorph :: Ff -> Int -> [(Ff,Ff)]
amorph g n = concatMap (\i -> map (\e -> (ex i 4,e)) (morph i g)) (afps n)

oc = groupBy (\(_,a) (_,b) -> (func a) == (func b)) . sortBy (\(_,a) (_,b) -> (compare (func a) (func b)))
oc2 x = map (\((e,f):r) -> (length r + 1,[f,e] ++ (map head $ group $ sort $ map fst r))) x 

main :: IO ()
main =
  do
    --print (asdi (ffl [7,1,2,6,3,5,4,0] "foo" (alphabet 3 2)) 4)
    print $ oc2 $ oc (amorph cnot 5) 
    -- print tofolli
    -- print fredkin
    -- print (asdi rot4 2)
    -- print (cmps [fredkin,tofolli] [fredkin,tofolli])

-- -- A bijection over a finite set with a name and explicit size. 
-- data Permutation = Permutation {
--                                    name::String,
--                                    alpha::[String],
--                                    size::Int,
--                                    func::(Int -> Int)
--                                }
-- instance Show Permutation where
--   show (Permutation name alphabet size f) = "\n" ++ sub 0 
--     where
--       sub n = if n == size then ""
--         else name ++ "( " ++ alphabet !! n ++ " ) = " ++ alphabet !! (f n) ++ "\n" ++ sub (n + 1)


-- permutationFromList :: [Int] -> String -> [String] -> Permutation
-- permutationFromList list name alpha = Permutation name alpha (length list) (\i -> list !! i) 

-- listFromPermutation :: Permutation -> [Int]
-- listFromPermutation (Permutation name alpha size f) = map f [0 .. size - 1]

-- -- The inverse of a permutation.
-- invertPermutation :: Permutation -> Permutation
-- invertPermutation p@(Permutation name alpha size f) =
--   Permutation (name ++ "_inverse") alpha size (\x -> fromJust $ elemIndex x (listFromPermutation p))


-- -- Composition.
-- composePermutations :: Permutation -> Permutation -> Permutation
-- composePermutations (Permutation gname alpha size g) (Permutation fname _ _ f) =
--   Permutation (fname ++ "_then_" ++ gname) alpha size (g . f)
  
-- -- All permutation of a alphabet of given size.
-- allPermutations :: Int -> [Permutation]
-- allPermutations n = map (\(i, p) -> permutationFromList p ("p" ++ show i ++ "of" ++ show size) (alphabet 1 n)) (zip [1 .. size] ps)
--   where
--     ps = permutations [0 .. n - 1]
--     size = length ps

-- -- Return all inversions of a permutation with respect to only the first n elements.
-- allInverses :: Int -> Permutation -> [Permutation]
-- allInverses n p@(Permutation name alpha size f) = filter (\q -> (take n $ listFromPermutation  (composePermutations q p)) == [ 0 .. n - 1 ]) (allPermutations size)
--   where
--     pinverse = invertPermutation p

-- allMorphs :: Permutation -> Permutation -> [Permutation]
-- allMorphs f g = map (\p -> composePermutations f (composePermutations g p)) (allInverses (size g) f)

-- -- invertPermutation :: [Int] -> [Int]
-- -- invertPermutation [] = []
-- -- invertPermutation p = map (\x -> fromJust $ elemIndex x p) [0 .. length p - 1]

-- -- composePermutation :: [Int] -> [Int] -> [Int]
-- -- composePermutation p1 = map (p1 !!)

-- -- mixPermutation :: [Int] -> [Int] -> [(Int,Int)]
-- -- mixPermutation p1 p2 = map (\x -> (p1 !! x, p2 !! x)) [0 .. length p1 - 1]
  
-- -- showPermutation :: [Int] -> String
-- -- showPermutation l = sub 0 l ++ "\n"
-- --   where
-- --     sub _ [] = ""
-- --     sub i (x : xs) = "f(" ++ show i ++ ") = " ++ show x ++ "\n" ++ sub (i + 1) xs

-- -- morphPermutation :: [Int] -> [Int] -> [Int]
-- -- morphPermutation f g = composePermutation f (composePermutation g (invertPermutation f))

-- -- occursCount :: Ord a => [a] -> [([a], Int)]
-- -- occursCount = map (\x -> ([head x], length x)) . group . sort

-- -- foc :: Int -> [([[Int]], Int)] -> [([[Int]], Int)]
-- -- foc _ [] = []
-- -- foc n (e@(_, x) : xs) = if x >= n then e : rest else rest
-- --   where
-- --     rest = foc n xs

-- -- allMorphs :: [Int] -> [[Int]]
-- -- allMorphs g = map (`morphPermutation` g) (permutations [0 .. length g - 1])



-- -- allMorphsOfAllFunctions :: Int -> [[[Int]]]
-- -- allMorphsOfAllFunctions n = map allMorphs ps
-- --   where
-- --     nmo = n - 1
-- --     ps = permutations [0 .. nmo]

-- main :: IO ()
-- main =
--   do
--     print (alphabet 3 2)
--     print fredkin
--     print $ listFromPermutation fredkin
--     print tofolli
--     print (composePermutations tofolli fredkin)
--     print rotate5
--     print (invertPermutation rotate5)
--     print (allPermutations 3)
--     print (allInverses 3 $ permutationFromList [1, 2, 0, 3] "foo" (alphabet 1 4))
--     print $ allMorphs fredkin rotate5
--        where
--          fredkin = permutationFromList [ 0, 1, 2, 3, 4, 6, 5, 7 ] "fredkin" (alphabet 3 2)
--          tofolli = permutationFromList [ 0, 1, 2, 3, 4, 5, 7, 6 ] "tofolli" (alphabet 3 2)
--          rotate5 = permutationFromList [ 1, 2, 3, 4, 0 ] "rotate5" (alphabet 1 5)
--   --   putStrLn $ showPermutation e1
--   --   putStrLn $ showPermutation ie1
--   --   putStrLn $ showPermutation e2
--   --   putStrLn $ showPermutation (composePermutation e2 ie1)
--   --   putStrLn $ showPermutation (morphPermutation e1 e2)
--   --   print (8 * 7 * 6 * 5 * 4 * 3 * 2) 
--   --   print $ occursCount $ allMorphs fredkin
--   --   print ( (mixPermutation [ 1, 2, 0 ] [ 2, 1, 0 ]))
--   -- where
--   --   e1 = [ 1, 0, 2 ]
--   --   ie1 = invertPermutation e1
--   --   e2 = [ 1, 2, 0 ]
--   --   toffoli = [ 0, 1, 2, 7, 4, 5, 6, 3 ]
--   --        000 001 010 011 100 101 110 111
--   --        000 001 010 111 100 101 110 011
--   --   fredkin = [ 0, 1, 2, 3, 4, 6, 5, 7 ]
  --        000 001 010 011 100 101 110 111
  --        000 001 010 011 100 110 101 011

type ListOfPrimes = [Int]
type Sum = Int
type MaxRange = Int

prime = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47] :: [Int]

master :: Int -> [(Int,Int)]
master max = findKeepers [] ((correct . suitEmUp) (findAllSums [] [] [1..max])) 


stripped max = map snd (premaster max)
premaster :: Int -> [(Int,Int)] 
premaster max = (correct . suitEmUp) (findAllSums [] [] [1..max])

correct :: [(Int, Int)] -> [(Int, Int)] 
correct = map subtr
    where subtr (a,b) = (a,b-a)

pair :: [(Int,Int)] -> [(Int, Int)]
pair list = [(a,b) | (b,a) <- list, (a,b) <- list] 

findKeepers :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findKeepers keepers [] = keepers
findKeepers keepers ((a,b):list) = findKeepers (new++keepers) list 
    where new = [(x,y) | (x,y) <- list, x==b, y==a]  

suitEmUp :: [Int] -> [(Int, Int)]
suitEmUp list = zip [1..(length list)] (reverse list)

findAllSums :: [Sum] -> ListOfPrimes -> [Int] -> [Sum]
findAllSums listOfSums _ [] = listOfSums
findAllSums listOfSums listofPrimes (l:listofInts)  
    | newSum == (l+1) = findAllSums (newSum:listOfSums) (listofPrimes++[l]) listofInts
    | True                  = findAllSums (newSum:listOfSums) listofPrimes     listofInts  
    where newSum = findFactorSum listofPrimes l

findFactorSum :: ListOfPrimes -> Int -> Int
findFactorSum _ 1 = 1
findFactorSum [] l = l+1 
findFactorSum (p:listofPrimes) l 
    | (fromIntegral p) > sqrt (fromIntegral l)   = findFactorSum [] l 
    | mod l p == 0 = findFactorSum2 0 p l
    | True         = findFactorSum listofPrimes l 

    where findFactorSum2 :: Int -> Int -> Int -> Int 
          findFactorSum2 count p l 
            | mod l p == 0 = findFactorSum2 (count+1) p (floor ((fromIntegral l)/(fromIntegral p)))
            | True         = (genSpecialSeries count p)*(findFactorSum listofPrimes l) 

genSpecialSeries :: Int -> Int -> Int
genSpecialSeries count prime = sum $ map (prime^) [0..count] 

--findFirstPrimeFactor :: ListofPrimes -> Int -> 





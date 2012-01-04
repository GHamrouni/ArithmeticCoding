import List  (findIndex, elemIndex)

-- |
-- Module      :  ArithmeticCoding
--
-- Maintainer  :  Hamrouni Ghassen <ghamrouni.iptech@gmail.com>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Haskell implementation of Arithmetic Coding

-- | A symbol in the alphabet
data Symbol  =  Symbol { name :: String } deriving (Show, Eq)

-- | An alphabet is a set of different symbols. Each symbol has a probablity of occurence.
--  
data Alphabet = Alphabet {  symbols  :: [Symbol]  ,  probability :: (Symbol -> Int)  }    
                                    
--                                         
cumulativeProbabilitiesAlphabet :: Alphabet -> [(Int, Int)]

cumulativeProbabilitiesAlphabet alphabet = 
        zip (cumulativeList) (tail cumulativeList)
        where 
                    cumulativeList = scanl (+)  0 probabilities
                    probabilities = (map (\x ->  (probability alphabet) x)) (symbols alphabet)
                                                                                                                                                                        
                                                                                    
-- |  Calulate the cumulative probabilities of the symbol Si and Si+1
cumulativeProbabilities :: Alphabet -> Int -> (Int, Int)

cumulativeProbabilities alphabet elementIndex = 
        (cumulativeProbabilitiesAlphabet alphabet) !! elementIndex

-- |  Calulate the new subinterval, based on cumulative probabilities
-- (l, h)    =    The current interval
-- c        =    the cumulative probabilities of the symbol Si
-- n        =    the cumulative probabilities of the symbol Si+1
-- t        =    the sum T of all probabilities
calculateInterval :: Int -> Int -> Int -> Int -> Int -> (Int, Int)

calculateInterval l h c n t  = (l + (div (c * (h - l)) t), l + ((div (n * (h - l)) t)))
    
-- | Calculate the sub interval for a particular symbol                
-- Using integer arithmetic instead of floating points                                            
subinterval :: Alphabet -> (Int, Int) -> Int -> (Int, Int)

subinterval alphabet (l, h) elementIndex = calculateInterval l h c n t
        where 
                (c, n) = cumulativeProbabilities alphabet elementIndex
                t = sum((map (\x ->  (probability alphabet) x)) (symbols alphabet))

                                                        
subintervalAlphabet :: Alphabet -> (Int, Int) -> [(Int, Int)]

subintervalAlphabet alphabet (l, h) = map ( \(c, n) -> (calculateInterval l h c n t)) cn
         where 
                    cn = cumulativeProbabilitiesAlphabet alphabet
                    t = sum((map (\x ->  (probability alphabet) x)) (symbols alphabet))            
                                                        

-- ------------------------------------------------------------
-- * Encoding
-- TODO: Interval shrinking
-- ------------------------------------------------------------                                                            
                                                                                                                                
-- | The arithmetic coding algorithm                                    
arithmeticEncode :: Alphabet -> [Int] -> (Int, Int) -> (Int, Int)

arithmeticEncode alphabet symbols currentInterval = 
        foldl (subinterval alphabet) currentInterval symbols

-- ------------------------------------------------------------
-- * Decoding
-- ------------------------------------------------------------

-- | Calculate the symbol    coresponding to an interval
-- Using integer arithmetic    instead of floating points                                            
decodeSymbol :: Alphabet -> (Int, Int) -> Int -> Maybe Int
decodeSymbol alphabet (l, h) p = let intervals = subintervalAlphabet alphabet (l, h) 
                                                  in findIndex (contains p) intervals

-- | decode a message
arithmeticDecode :: Alphabet -> (Int, Int) -> (Int, Int) -> Int -> [Int]

arithmeticDecode alphabet currentInterval (a, b) terminationSymbol = 
        case decodedSymbol of
                (Just n) -> 
                                if (n == terminationSymbol) then [n] 
                                else ([n] ++ (arithmeticDecode alphabet (subinterval alphabet currentInterval n)  (a, b) terminationSymbol))
                otherwise -> []
        where
                    decodedSymbol = decodeSymbol alphabet currentInterval (div (a + b) 2) 


-- ------------------------------------------------------------
-- * Utilities
-- ------------------------------------------------------------
                    
-- | Utilities functions
contains ::  (Ord t) => t -> (t, t) -> Bool
contains c (a, b) = (c >= a) && (c < b)






                                                        
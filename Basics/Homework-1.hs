module Main where
-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf 


--Exercise 1
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits number 
	| number < 0 = []
	| otherwise = intParserToList(number)

intParserToList :: Integer -> [Integer]
intParserToList number
	| number == 0 = []	
	| number > 0 =  intParserToList((number `div` 10)) ++ [(number `mod` 10)] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev number = reverse (toDigits number)

--Exercise 2
--Example doubleEveryOther [1,2,3,4] -> [1,2,6,4]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther number = reverse(doubleList(reverse number))

doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:[]) = [x]
doubleList (x:(y:zs)) = x : (y * 2) : doubleList(zs)

--Exercise 3
--Example sumDigits [16,7,12,5] = 1+6+7+1+2+5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:y)
	| x > 9 = (x `mod` 10) + (x `div` 10) + sumDigits(y) 
	| otherwise = x + sumDigits(y) 

--Exercise 4
--Example validate  4012888888881881 = True
validate :: Integer -> Bool
validate number
	| sumDigits(doubleEveryOther(toDigits(number))) `mod` 10 == 0 = True
	| otherwise = False


--Exercise 5 - Tower of Hanoi 
--Example hanoi 2 "a" "b" "c" == [("a","c"),("a","b"),("c","b")]
--2 -> 3 moves
--3 -> hanoi 3 "a" "b" "c" == [("a","b"),("a","c"),("b","c"),("a","b"),("b","c"),("b","c"),("a","b")] 
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi number peg1 peg2 peg3 = [("peg1","peg2"),("peg2","peg3")]




main ::  IO ()
main = print (toDigits 52576)






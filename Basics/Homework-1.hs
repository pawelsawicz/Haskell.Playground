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

main ::  IO ()
main = print (toDigits 52576)
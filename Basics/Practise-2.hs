
--enumeration types

data Thing = Shoe
	| Ship
	| SealingWax
	| Cabbage
	| King
 deriving Show

data Human = Student
	| Policeman
	| Doctor
 deriving Show

-- variable of custom type
student :: Human
student = Student

--variable
studentAge :: Int
studentAge = 6

--list of custom types
listOfHumans :: [Human]
listOfHumans = [Student, Policeman, Doctor]

--patter-matching for custom types
isPayable :: Human -> Bool
isPayable Student = False
isPayable Policeman = True
isPayable Doctor = True

--patter-matching
isNumberOneOrTwo :: Int -> String
isNumberOneOrTwo 1 = "One"
isNumberOneOrTwo 2 = "Two"

-- patter-matching with proper function order call
isPayableByCallOrder :: Human -> Bool
isPayableByCallOrder Student = False
isPayableByCallOrder _ = True

--enumeration type with constructor that takes argument

data FailableDouble = Failure
	| OK Double
 deriving Show

-- usage of that enum type with constructor
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

data RestrictedKeyword = Restricted
	| Allowed String
	deriving Show

checkKeyword :: String -> RestrictedKeyword
checkKeyword "Bad Word" = Restricted
checkKeyword name = Allowed name

--more patter-matching!
valueOfKeyword :: RestrictedKeyword -> String
valueOfKeyword Restricted = "Restricted"
valueOfKeyword (Allowed s) =  s

-- moar arguments in constructors!
-- Store a person's name, age, profession

data Profession = Firefighter
	| Lecturer
	| Grocerer
	deriving Show

data Position = Chief
	| Officer
	| Ordinary 

data Person = Person String Int Profession
	| NotEmployedPerson String Int
	deriving Show

aSmith :: Person
aSmith = Person "Adam Smith" 20 Lecturer

aStan :: Person
aStan = Person "Adam Stan" 21 Firefighter

aMile :: Person
aMile = NotEmployedPerson "Ani Milo" 20

--mStock :: Person
--mStock = "Mikel Stock" 25 Grocerer

getAge :: Person -> Int
getAge (Person _ a _) = a
getAge (NotEmployedPerson _ a) = a

--Moar patter-matching

data AbstractType = Constr1 String String
	| Constr2 String
	| Constr3 String String Int
	| Constr4
	| Constr5 String Int
	deriving Show

abstractObject :: AbstractType
abstractObject = Constr1 "Pawel" "Sawicz"


-- _ wildcard pattern
-- x@patt by x you have access to whole value of type
-- more specific patters have to be in upper stack coz check runs from top to bottom

abstractPatternMatching :: AbstractType -> String
abstractPatternMatching x@(Constr1 "Adi" "Adams") = show x ++ " You are from our family!"
abstractPatternMatching (Constr1 a "Adams") = a ++ " You are from Adams family !"
abstractPatternMatching (Constr1 a b) = a ++ b
abstractPatternMatching (Constr5 a _) = a
abstractPatternMatching _ = "Not defined pattern"

-- case expression example
ex03 = case "Hello" of 
	[] -> 3
	('H':s) -> length s
	_ -> 7

--isPayable example implemented by case expr.
isPayableByExpression :: Human -> Bool
isPayableByExpression h = case h of
	Student -> False
	_ -> True

--Recursive data typed
--

data IntList = Empty
	| Cons Int IntList
	deriving Show

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

--binary tree made up by recursive data

main :: IO()
main = print("Practise")

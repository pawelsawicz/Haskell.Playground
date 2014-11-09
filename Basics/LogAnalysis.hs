{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


--Exercise 1
-- Parse sigle line of log for example "E 2 562 help help" -> (Error 2) 562 "help help"
parseMessage :: String -> LogMessage
parseMessage [] = Unknown "Empty"
parseMessage a@(x:_)
	| isProperFormat(x) = LogMessage (getMessageType(a)) 555 a
	| otherwise = Unknown a


getMessageType :: String -> MessageType
getMessageType [] = Info -- think how to remove it ?
getMessageType (x:_) = case x of
	'I' -> Info
	'W' -> Warning
	('E') -> Error 55
	_ -> Info -- think how to remove it ?

getTimeStamp :: String -> TimeStamp
getTimeStamp stringValue = case reads stringValue :: [(Int,String)] of	
	[(n,_)] -> n
	_ -> 0
	
getRestOfMessage :: String -> String
getRestOfMessage stringValue = case reads stringValue :: [(Int,String)] of	
	[(_,n)] -> n
	_ -> "Empty"

isProperFormat :: Char -> Bool
isProperFormat x = case x of
	'I' -> True
	'W' -> True
	'E' -> True
	_ -> False

parseMess :: String -> LogMessage
parseMess [] = Unknown "Empty"
parseMess (x:s) = case x of
	'I' -> LogMessage Info (getTimeStamp(s)) (getRestOfMessage(s))
	'W' -> LogMessage Warning (getTimeStamp(s)) (getRestOfMessage(s))
	'E' -> LogMessage (Error ) 
	_ -> Unknown "Empty"
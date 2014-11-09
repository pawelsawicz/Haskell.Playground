{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


--Exercise 1
-- Parse sigle line of log for example "E 2 562 help help" -> (Error 2) 562 "help help"
parseMessage :: String -> LogMessage
parseMessage [] = Unknown "Empty"
parseMessage (x:s) = case x of
	'I' -> LogMessage Info (getTimeStamp(s)) (getRestOfMessage(s))
	'W' -> LogMessage Warning (getTimeStamp(s)) (getRestOfMessage(s))
	'E' -> LogMessage (Error (getErrorValue(s))) (getTimeStamp(getRestOfMessage(s))) (getRestOfMessage(getRestOfMessage(s)))
	_ -> Unknown "Empty"

getTimeStamp :: String -> TimeStamp
getTimeStamp stringValue = case reads stringValue :: [(Int,String)] of	
	[(n,_)] -> n
	_ -> 0

getErrorValue :: String -> Int
getErrorValue stringValue = case reads stringValue :: [(Int,String)] of	
	[(n,_)] -> n
	_ -> 0

getRestOfMessage :: String -> String
getRestOfMessage stringValue = case reads stringValue :: [(Int,String)] of	
	[(_,n)] -> n
	_ -> "Empty"

parse :: String -> [LogMessage]
parse valueOfFile = map (parseMessage) (lines valueOfFile)


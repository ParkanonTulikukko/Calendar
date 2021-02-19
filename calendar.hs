import Date

data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)

instance Show EventInfo where
  show (EventInfo name place date) = "Event " ++ name ++ " happens at " ++ place ++ " on " ++ (show date)

main = loop $ return []
 
loop :: IO [EventInfo] -> IO ()
loop ioEvents =
 do
 input <- getLine
 if input == "Quit"
   then putStrLn "bye"
   else doCommand input ioEvents

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
  events <- ioEvents --Now you can use events as [EventInfo]
  case checkInput (words input) of
        "ADD"   -> do
                    putStrLn "ok"
                    loop $ return (addEvent (getEventInformation (words input)) events) 
        "TELL"  -> do   
                    putStrLn "TELL"
                    putStrLn $ printEvents events 
                    loop $ return events 
        "DATE"  -> do
                    putStrLn "DATE"
                    putStrLn $ printEvents events  
                    loop $ return events       
        "PLACE" -> do
                    putStrLn "PLACE"
                    putStrLn $ printEvents events 
                    loop $ return events        
        "ERROR" -> do
                    putStrLn "I do not understand that. I understand the following:\n\
                        \*Event <name> happens at <place> on <date>\n\
                        \*Tell me about <eventname>\n\ 
                        \*What happens on <date>\n\
                        \*What happens at <place>\n\
                        \*Quit"  
                    loop $ return events 

checkInput :: [String] -> String
checkInput input = 
    case input!!0 of
        "Event" -> checkEvent input
        "Tell" -> checkEventNameQuery input
        "What" -> if checkDateQuery input == "DATE" then "DATE" 
                  else 
                    if checkPlaceQuery input == "PLACE" then "PLACE" 
                    else "ERROR"
        _ -> "ERROR"      

--SEMMONEN TARKISTUS VOIS OLLA, ETTÄ KOKO RIMPSU switch caseen, JOSSA
--KATOTTAIS TRUE FALSE FUNKTIOIDEN KANSSA LÄPI MINKÄLAINEN SYÖTE ON KYSEESSÄ

checkEvent :: [String] -> String
checkEvent input = if input!!2 == "happens" && input!!3 == "at" && input!!5 == "on" && length input == 7 
               then "ADD" 
               else "ERROR"   

checkEventNameQuery :: [String] -> String
checkEventNameQuery input = if input!!1 == "me" && input!!2 == "about" && length input == 4 
               then "TELL"
               else "ERROR"

checkDateQuery :: [String] -> String
checkDateQuery input = if input!!1 == "happens" && input!!2 == "on" && length input == 4 
              then "DATE" 
              else "ERROR"

checkPlaceQuery :: [String] -> String
checkPlaceQuery input = if input!!1 == "happens" && input!!2 == "at" && length input == 4 
              then "PLACE" 
              else "ERROR"

printEvents :: [EventInfo] -> String
printEvents [] = "tyhjä"
printEvents (eventInfo:xs) = show eventInfo  

getEventInformation :: [String] -> [String] 
getEventInformation input = [input!!1] ++ [input!!4] ++ [input!!6]   

addEvent :: [String] -> [EventInfo] -> [EventInfo]
addEvent [name, place, date] events = events ++ [EventInfo name place (getDate (deleteQuatations date))] --(makeDate 2019 10 08)]  

deleteQuatations :: String -> String
deleteQuatations s = init(tail s) 

--'2019-10-08'
getDate :: String -> Date
getDate date = makeDate (read(take 4 date)::Integer) (read(take 2(drop 5 date))::Integer) (read(drop 8 date)::Integer)  

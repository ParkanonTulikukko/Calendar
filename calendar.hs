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
                    putStrLn $ printByEvent ((words input)!!3) events 
                    loop $ return events 
        "DATE"  -> do
                    putStrLn $ printByDate (deleteQuatations ((words input)!!3)) events 
                    loop $ return events       
        "PLACE" -> do
                    putStrLn $ printByPlace (deleteQuatations ((words input)!!3)) events 
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

printByEvent :: String -> [EventInfo] -> String
printByEvent event [] = "I do not know of such event"
printByEvent event (eventInfo:xs) = if (name eventInfo) == event then show eventInfo else printByEvent event xs    

printByDate :: String -> [EventInfo] -> String
printByDate dateInput [] = "Nothing that I know of"
printByDate dateInput (eventInfo:xs) = if show (date eventInfo) == dateInput then show eventInfo else printByDate dateInput xs   

printByPlace :: String -> [EventInfo] -> String
printByPlace placeInput [] = "Nothing that I know of"
printByPlace placeInput (eventInfo:xs) = if (place eventInfo) == placeInput then show eventInfo else printByPlace placeInput xs   

getEventInformation :: [String] -> [String] 
getEventInformation input = [input!!1] ++ [input!!4] ++ [input!!6]   

addEvent :: [String] -> [EventInfo] -> [EventInfo]
addEvent [name, place, date] events = events ++ [EventInfo (deleteQuatations name) (deleteQuatations place) (getDate (deleteQuatations date))] --(makeDate 2019 10 08)]  

deleteQuatations :: String -> String
deleteQuatations s = init(tail s) 

--'2019-10-08'
getDate :: String -> Date
getDate date = makeDate (read(take 4 date)::Integer) (read(take 2(drop 5 date))::Integer) (read(drop 8 date)::Integer)  

{-- 
TEST DATA
eventit = (addEvent (getEventInformation (words "Event 'jesse' happens at 'vastis' on '2020-12-09'"))(addEvent(getEventInformation (words "Event 'jesse' happens at 'vastis' on '2030-01-02'")) []))

--}
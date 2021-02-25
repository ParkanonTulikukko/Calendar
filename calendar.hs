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
  putStrLn (show events)
  case checkInput (split input) of
        "ADD"   -> do
                    putStrLn "ok"
                    loop $ return (addEvent (getEventInformation (split input)) events) 
        "TELL"  -> do   
                    putStrLn $ printByEvent ((split input)!!1) events 
                    loop $ return events 
        "DATE"  -> do
                    putStrLn $ printByDate ((split input)!!1) events 
                    loop $ return events       
        "PLACE" -> do
                    putStrLn $ printByPlace ((split input)!!1) events 
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
checkInput [] = "ERROR"
checkInput input = 
    case input!!0 of
        "Event" -> checkEvent input
        "Tell me about" -> checkEventNameQuery input
        "What happens at" -> if length input == 2 then "PLACE" else "ERROR" 
        "What happens on" -> if length input == 2 then "DATE" else "ERROR"
        _ -> "ERROR"      

--Input example: ["Event", "sika pailut", "happens at",  "vastiksen terangi", "on", "2020-10-10"] 
checkEvent :: [String] -> String
checkEvent input = if input!!2 == "happens at" && input!!4 == "on" && length input == 6
               then "ADD" 
               else "ERROR"   

--Input example: ["Tell me about", "sika pailut"]
checkEventNameQuery :: [String] -> String
checkEventNameQuery input = if input!!0 == "Tell me about" && length input == 2 
               then "TELL"
               else "ERROR"

--Input example: ["What happens on", "2020-10-10"]
checkDateQuery :: [String] -> String
checkDateQuery input = if input!!0 == "What happens on" && length input == 2
              then "DATE" 
              else "ERROR"

--Input example: ["What happens at", "vastiksen terangi"]
checkPlaceQuery :: [String] -> String
checkPlaceQuery input = if input!!0 == "What happens at" && length input == 2 
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

--Example: ["Event","jesse","happens at","vastis","on","2020-12-09"]
getEventInformation :: [String] -> [String] 
getEventInformation input = [input!!1] ++ [input!!3] ++ [input!!5]   

addEvent :: [String] -> [EventInfo] -> [EventInfo]
addEvent [name, place, date] events = events ++ [EventInfo name place (getDate date)] 

deleteQuatations :: String -> String
deleteQuatations s = init(tail s) 

--'2019-10-08'
getDate :: String -> Date
getDate date = makeDate (read(take 4 date)::Integer) (read(take 2(drop 5 date))::Integer) (read(drop 8 date)::Integer)  

split :: String -> [String]
split "" = []
split xs = case break (=='\'') xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> trimString ls : split rs

trimString :: String -> String
trimString s = if (head s) == ' ' then
                  trimString (tail s)
                else
                  if last s == ' ' then trimString (init s)  
                  else s

{-- 
TEST DATA
eventit = (addEvent (getEventInformation (split "Event 'jesse' happens at 'vastis' on '2020-12-09'"))(addEvent(getEventInformation (split "Event 'jesse' happens at 'vastis' on '2030-01-02'")) []))
--}
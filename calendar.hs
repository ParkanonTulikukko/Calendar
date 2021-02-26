--import Date
{-
main = loop $ return []
 
loop :: IO [EventInfo] -> IO ()
loop ioEvents =
 do
 input <- getLine
 if input == "Quit"
   then putStrLn "bye"
   else doCommand input ioEvents

data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)
-}

instance Show EventInfo where
  show (EventInfo name place date) = "Event " ++ name ++ " happens at " ++ place ++ " on " ++ (show date)

{-TEST DATA
 \*Event <name>            happens at <place>     on <date>\n\
Event 'vanhan koulun humpat' happens at 'piki linna' on '2020-10-10'   
Event 'vanhan koulun humpat' happens at 'piki linna' on '2020-07-07'
Event 'lekurille' happens at 'tammela keskus' on '2030-01-05'
Tell me about 'vanhan koulun humpat'
-}  

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
  events <- ioEvents --Now you can use events as [EventInfo]
  --putStrLn (show events)
  case checkInput (split input) of
        "ADD"   -> do
                    if checkDateInput ((split input)!!5) == False then 
                      do
                      putStrLn "Bad date"
                      loop $ return events 
                    else
                      do
                      putStrLn "ok"
                      loop $ return (addEvent (getEventInformation (split input)) events) 
                      
        "TELL"  -> do   
                    putStrLn $ printByEvent ((split input)!!1) events 
                    loop $ return events 
        "DATE"  -> do
                    --printByDate palauttais niinku suoraan tommosen putStrLn olion!
                    --sillon ei tässä sitä tarvis kirjoittaa
                    if (getByDate ((split input)!!1) events == []) then
                      putStrLn $ "Nothing that I know of"
                    else     
                      putStr (unlines (getByDate ((split input)!!1) events)) 
                    loop $ return events       
        "PLACE" -> do 
                    if (getByPlace ((split input)!!1) events == []) then
                      putStrLn $ "Nothing that I know of"
                    else 
                      putStr (unlines (getByPlace ((split input)!!1) events))
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

checkDateInput :: String -> Bool
checkDateInput date = correctDate (read(take 4 date)::Integer) (read(take 2(drop 5 date))::Integer) (read(drop 8 date)::Integer) 

printByEvent :: String -> [EventInfo] -> String
printByEvent event [] = "I do not know of such event"
printByEvent event (eventInfo:xs) = if (name eventInfo) == event then show eventInfo else printByEvent event xs    

--"What happens on '2020-10-10'"
--"Event Event A happens on 2001-02-03"
getByDate :: String -> [EventInfo] -> [String]
getByDate dateInput [] = []
getByDate dateInput (eventInfo:xs) = 
  if show (date eventInfo) == dateInput then 
    quicksort (["Event " ++ name eventInfo ++ " happens on " ++ dateInput] ++ getByDate dateInput xs) 
  else getByDate dateInput xs   

{--
Event 'vanhan koulun humpat' happens at 'piki linna' on '2020-10-10'
Event 'sikamaiset pileet' happens at 'piki linna' on '2020-10-10'
Event 'aamujumppa' happens at 'piki linna' on '2020-10-10'
What happens at 'piki linna'
Event Event A happens at piki linna --}
--printByPlace :: String -> [EventInfo] -> String
getByPlace :: String -> [EventInfo] -> [String]
--printByPlace x z = putStr ( unlines ["1","2","3","4"] )
getByPlace placeInput [] = []
getByPlace placeInput (eventInfo:xs) = 
  if (place eventInfo) == placeInput then 
    quicksort (["Event " ++ name eventInfo ++ " happens at " ++ placeInput] ++ getByPlace placeInput xs)
  else getByPlace placeInput xs   

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

--Example: ["Event","jesse","happens at","vastis","on","2020-12-09"]
getEventInformation :: [String] -> [String] 
getEventInformation input = [input!!1] ++ [input!!3] ++ [input!!5]   

addEvent :: [String] -> [EventInfo] -> [EventInfo]
addEvent [newEventName, newEventPlace, newEventDate] [] = [EventInfo newEventName newEventPlace (getDate newEventDate)] 
addEvent [newEventName, newEventPlace, newEventDate] (x:xs) =
  if newEventName == name x then 
    [EventInfo newEventName newEventPlace (getDate newEventDate)] ++ xs
  else
    [x] ++ addEvent [newEventName, newEventPlace, newEventDate] xs    
--events ++ [EventInfo name place (getDate date)] 

deleteQuatations :: String -> String
deleteQuatations s = init(tail s) 

--'2019-10-08'
getDate :: String -> Date
getDate date = makeDate (read(take 4 date)::Integer) (read(take 2(drop 5 date))::Integer) (read(drop 8 date)::Integer)  

split :: String -> [String]
split " " = []
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
input = Event 'vanhankoulunhumpat' happens at 'pikilinna' on '2020-10-10'

checkInput 
eventit = (addEvent (getEventInformation (split "Event 'jesse' happens at 'vastis' on '2020-12-09'"))(addEvent(getEventInformation (split "Event 'jesse' happens at 'vastis' on '2030-01-02'")) []))
--}

data Month = MakeMonth Integer deriving (Eq, Show, Ord)

toMonth :: Integer -> Month
toMonth x
 | x < 1 = error "Minimum month number is 1" 
 | x > 12 = error "Maximum month number is 12" 
 | otherwise = MakeMonth x

fromMonth :: Month -> Integer
fromMonth (MakeMonth i) = i -- Pattern match i out 

fromMonthToString :: Month -> String
fromMonthToString (MakeMonth i) = if i < 10 then "0" ++ show i else show i  

-- I do not define abs or signum. It seems to go through.
-- I only allow positive values so they are not relevant.

instance Num Month where
    fromInteger = toMonth
    x + y = 
        let r = fromMonth x + fromMonth y 
        in if r < 1 || r > 12 then error "Unnatural addition for month" else toMonth r
    x - y = 
        let r = fromMonth x - fromMonth y 
        in if r < 1 || r > 12 then error "Unnatural subtraction for month" else toMonth r
    x * y = 
        let r = fromMonth x * fromMonth y 
        in if r < 1 || r > 12 then error "Unnatural multiplication for month" else toMonth r

data Day = MakeDay Integer deriving (Eq, Show, Ord)

toDay :: Integer -> Day
toDay x
 | x < 1 = error "Minimum day number is 1" 
 | x > 31 = error "Maximum day number is 31" 
 | otherwise = MakeDay x

fromDay :: Day -> Integer
fromDay (MakeDay i) = i 

instance Num Day where
 fromInteger = toDay
 x + y = toDay $ fromDay x + fromDay y
 x - y = toDay $ fromDay x - fromDay y
 x * y = toDay $ fromDay x * fromDay y


{- this would take care of year 0
data Year = MakeYear Integer deriving (Eq, Show)

toYear :: Integer -> Year
toYear x
 | x == 0 = error "No year 0" 
 | otherwise = MakeYear x

fromYear :: Year -> Integer
fromYear (MakeYear i) = i 

instance Num Year where
 fromInteger = toYear
 x + y = toYear $ fromYear x + fromYear y 
 x - y = toYear $ fromYear x - fromYear y 
 x * y = toYear $ fromYear x * fromYear y

-}

-- This does not take care of year 0 ie it allows it

newtype Year = MakeYear Integer deriving (Eq, Show, Ord, Read)

toYear :: Integer -> Year
toYear x
 | x == 0 = error "No year 0"
 | otherwise = MakeYear x

fromYear :: Year -> Integer
fromYear (MakeYear x) = x

fromDayToString :: Day -> String
fromDayToString (MakeDay i) = if i < 10 then "0" ++ show i else show i  

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord)
instance Show Date where
  show (Date year month day) = show (fromYear year) ++ "-" ++  (fromMonthToString month) ++ "-" ++ (fromDayToString day) 

getYear :: Date -> Integer
getYear (Date year month day) = fromYear year

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

-- A function to check if a year is a leap year

leapYear (MakeYear y)
 | mod y 400 == 0 = True
 | mod y 100 == 0 = False
 | mod y 4 == 0 = True
 | otherwise = False

makeMaybeDate :: Integer -> Integer -> Integer -> Maybe Date
makeMaybeDate y m d
 | y == 0 = Nothing
 | elem m [1,3,5,7,8,10,12] &&
 elem d [1..31] = makeJustDate y m d
 | elem m [4,6,9,11] &&
 (elem d [1..30]) = makeJustDate y m d
 | m==2 && elem d [1..28] = makeJustDate y m d
 | leapYear (toYear y) && m==2 && d==29 = makeJustDate y m d
 | otherwise = Nothing
 where makeJustDate y m d = Just Date {year = toYear y, month = toMonth m, day = toDay d}

-- 3: Write a function to check if a given date (y,m,d)
-- is correct

correctDate :: Integer -> Integer -> Integer -> Bool
correctDate 0 _ _ = False
correctDate y m d
 | (elem m [1,3,5,7,8,10,12]) && (elem d [1..31]) = True
 | (elem m [4,6,9,11]) && (elem d [1..30]) = True
 | (m==2) && (elem d [1..28]) = True
 | (leapYear (toYear y)) && (m==2) && (d==29) = True
 | otherwise = False

makeDate :: Integer -> Integer -> Integer -> Date
makeDate y m d
 | correctDate y m d = Date { year = toYear y, month = toMonth m, day = toDay d }
 | otherwise = error "not correct combination of integers for year, month and day"

-- 4: Write a function that, given a date,
-- calculates the next date

nextDate :: Date -> Date
nextDate date
 | correctDate y m (d+1) = Date { year = year date, month = month date, day = toDay (d+1) }
 | correctDate y (m+1) 1 = Date { year = year date, month = toMonth (m+1), day = toDay 1 }
 | y == (-1) = Date { year = toYear 1, month = toMonth 1, day = toDay 1 }
 | otherwise = Date { year = toYear (y+1), month = toMonth 1, day = toDay 1 }
 where 
    y = fromYear $ year date
    m = fromMonth $ month date
    d = fromDay $ day date
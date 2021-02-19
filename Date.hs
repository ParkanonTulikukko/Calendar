module Date (
Date,   
makeDate
) where

-- Note: this is not a complete implementation for dates.
-- This is just a data type example.

-- Define safer date types than (Int,Int,Int)

data Month = MakeMonth Integer deriving (Eq, Show, Ord)

toMonth :: Integer -> Month
toMonth x
 | x < 1 = error "Minimum month number is 1" 
 | x > 12 = error "Maximum month number is 12" 
 | otherwise = MakeMonth x

fromMonth :: Month -> Integer
fromMonth (MakeMonth i) = i -- Pattern match i out 

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

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord)
instance Show Date where
  show (Date year month day) = "'" ++ show (fromYear year) ++ "-" ++  show (fromMonth month) ++ "-" ++ show (fromDay day) ++ "'" 

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